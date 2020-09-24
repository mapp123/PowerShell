# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

<#
.SYNOPSIS
Downloads to packages from PowerShell Gallery which are missing from the Azure DevOps Artifacts feed.

.PARAMETER AzureDevOpsPAT
PAT for the username used for authenticating to the Azure DevOps Artifacts feed.

.PARAMETER Destination
Path to the folder where the packages should be stored for uploading to Azure DevOps Artifacts feed.

#>
function SyncGalleryToAzArtifacts {
    param(
        [Parameter(Mandatory = $true)] [string] $AzDevOpsFeedUserName,
        [Parameter(Mandatory = $true)] [string] $AzDevOpsPAT,
        [Parameter(Mandatory = $true)] [string] $Destination
    )

    $csproj = [xml] (Get-Content 'src/Modules/PSGalleryModules.csproj')
    $packages = @($csproj.Project.ItemGroup.PackageReference | ForEach-Object { [ordered] @{Name = $_.Include; Version = $_.Version }})

    $galleryPackages = @()
    $azArtifactsPackages = @()
    $modulesToUpdate = @()

    $galleryUrl = 'https://www.powershellgallery.com/api/v2/'
    $azArtifactsUrl = 'https://mscodehub.pkgs.visualstudio.com/_packaging/pscore-release/nuget/v2'

    $azDevOpsCreds = [pscredential]::new($AzDevOpsFeedUserName, (ConvertTo-SecureString -String $AzDevOpsPAT -AsPlainText -Force))

    foreach ($package in $packages) {
        $packageName = $package.Name

        try {
            # Get module from gallery
            $foundPackageOnGallery = Find-Package -ProviderName NuGet -Source $galleryUrl -AllVersions -Name $package.Name -Force -AllowPreReleaseVersion
            $galleryTarget = GetTargetPackages -packages $foundPackageOnGallery

            Write-Verbose -Verbose "Found module $packageName - $($galleryTarget.OutString) in gallery"
            $galleryPackages += $galleryTarget
        } catch {
            if ($_.FullyQualifiedErrorId -eq 'NoMatchFoundForCriteria,Microsoft.PowerShell.PackageManagement.Cmdlets.FindPackage') {
                # Log and ignore failure is required version is not found on gallery.
                Write-Warning "Module not found on gallery $packageName - $($package.Version)"
            }
            else {
                Write-Error $_
            }
        }

        try {
            # Get module from Az Artifacts
            # There seems to be a bug in the feed with RequiredVersion matching. Adding workaround with post filtering.
            # Issue: https://github.com/OneGet/oneget/issues/397
            $foundPackageOnAz = Find-Package -ProviderName NuGet -Source $azArtifactsUrl -AllVersions -Name $package.Name -Force -Credential $azDevOpsCreds -AllowPreReleaseVersion
            $azTarget = GetTargetPackages -packages $foundPackageOnAz

            Write-Verbose -Verbose "Found module $packageName - $($azTarget.OutString) in azArtifacts"
            $azArtifactsPackages += $azTarget
        } catch {
            if ($_.FullyQualifiedErrorId -eq 'NoMatchFoundForCriteria,Microsoft.PowerShell.PackageManagement.Cmdlets.FindPackage') {
                # Log and add the module to update list.
                Write-Verbose -Verbose "Az Artifacts Module needs update to - $packageName - $($package.Version)"
                $modulesToUpdate += $package
            }
            else {
                Write-Error $_
            }
        }

        if ($galleryTarget.LatestStable -and !$azTarget.LatestStable) {
            Write-Verbose -Verbose "Module needs to be updated $packageName - $($galleryTarget.LatestStable.Version)"
            $modulesToUpdate += $galleryTarget.LatestStable
        }

        if ($galleryTarget.LatestPreview -and !$azTarget.LatestPreview) {
            Write-Verbose -Verbose "Module needs to be updated $packageName - $($galleryTarget.LatestPreview.Version)"
            $modulesToUpdate += $galleryTarget.LatestPreview
        }

        if ($galleryTarget.LatestStable -and $azTarget.LatestStable) {
            $pkgOnGalleryVersion = [semver]::new($galleryTarget.LatestStable.Version)
            $pkgOnAzVersion = [semver]::new($azTarget.LatestStable.Version)

            if ($pkgOnAzVersion -lt $pkgOnGalleryVersion) {
                Write-Verbose -Verbose "Module needs to be updated $packageName - $($galleryTarget.LatestStable.Version)"
                $modulesToUpdate += $galleryTarget.LatestStable
            }
            elseif ($pkgOnGalleryVersion -lt $pkgOnAzVersion) {
                Write-Warning "Newer stable version found on Az Artifacts - $packageName - $($azTarget.LatestStable.Version)"
            }
            else {
                Write-Verbose -Verbose "Module is in sync for stable version - $packageName"
            }
        }

        if ($galleryTarget.LatestPreview -and $azTarget.LatestPreview) {
            $pkgOnGalleryVersion = [semver]::new($galleryTarget.LatestPreview.Version)
            $pkgOnAzVersion = [semver]::new($azTarget.LatestPreview.Version)

            if ($pkgOnAzVersion -lt $pkgOnGalleryVersion) {
                Write-Verbose -Verbose "Module needs to be updated $packageName - $($galleryTarget.LatestPreview.Version)"
                $modulesToUpdate += $galleryTarget.LatestPreview
            }
            elseif ($pkgOnGalleryVersion -lt $pkgOnAzVersion) {
                Write-Warning "Newer preview version found on Az Artifacts - $packageName - $($azTarget.LatestPreview.Version)"
            }
            else {
                Write-Verbose -Verbose "Module is in sync for preview version - $packageName"
            }
        }
    }

    "`nGallery Packages:"
    $galleryPackages.OutString

    "`nAz Artifacts Packages:`n"
    $azArtifactsPackages.OutString

    "`nModules to update:`n"
    $modulesToUpdate

    foreach ($package in $modulesToUpdate) {
        Write-Verbose -Verbose "Saving package $($package.Name) - $($package.Version)"
        Save-Package -Provider NuGet -Source $galleryUrl -Name $package.Name -RequiredVersion $package.Version -Path $Destination
    }

    if ($modulesToUpdate.Length -gt 0)
    {
        # Remove dependent packages downloaded by Save-Package if there are already present in AzArtifacts feed.
        try {
            $null = Register-PackageSource -Name local -Location $Destination -ProviderName NuGet -Force
            $packageNamesToKeep = @()
            $savedPackages = Find-Package -Source local -AllVersions -AllowPreReleaseVersion

            Write-Verbose -Verbose "Saved packages:"
            $savedPackages | Out-String | Write-Verbose -Verbose

            foreach($package in $savedPackages) {
                $pkgVersion = NormalizeVersion -version $package.Version
                $foundMatch = $azArtifactsPackages | Where-Object { $_.Name -eq $package.Name -and (NormalizeVersion -version $_.Version) -eq $pkgVersion }

                if(-not $foundMatch) {
                    Write-Verbose "Keeping package $($package.PackageFileName)" -Verbose
                    $packageNamesToKeep += "{0}*.nupkg" -f $package.Name
                }
            }

            if ($packageNamesToKeep.Length -gt 0) {
                ## Removing only if we do have some packages to keep,
                ## otherwise the '$Destination' folder will be removed.
                Remove-Item -Path $Destination -Exclude $packageNamesToKeep -Recurse -Force -Verbose
            }

            Write-Verbose -Verbose "Packages kept for upload"
            Get-ChildItem $Destination | Out-String | Write-Verbose -Verbose
        }
        finally {
            Unregister-PackageSource -Name local -Force -ErrorAction SilentlyContinue
        }
    }
}

function GetTargetPackages {
    param(
        [Microsoft.PackageManagement.Packaging.SoftwareIdentity[]]
        $packages
    )

    $latestStable = $allPackages | Where-Object { -not $_.Version.Contains('-') } `
        | Sort-Object -Descending -Property Version `
        | Select-Object -First 1

    $latestPreview = $allPackages | Where-Object { $_.Version.Contains('-') } `
        | Sort-Object -Descending -Property Version `
        | Select-Object -First 1

    if ($latestStable -and $latestPreview) {
        $previewStablePart = ($latestPreview.Version -split '-')[0]
        if ($previewStablePart -le $latestStable.Version) {
            $latestPreview = $null
        }
    }

    if ($latestStable) {
        $outString = "Latest stable version: " + $latestStable.Version
    }

    if ($latestPreview) {
        if ($outString) { $outString += "; " }
        $outString += "Latest preview version: " + $latestPreview.Version
    }

    Write-Output @{ LatestStable = $latestStable; LatestPreview = $latestPreview; OutString = $outString }
}

function NormalizeVersion {
    param ([string] $version)

    $sVer = if ($version -match "(\d+.\d+.\d+).0") {
        $Matches[1]
    } elseif ($version -match "^\d+.\d+$") {
        # Two digit versions are stored as three digit versions
        "$version.0"
    } else {
        $version
    }

    $sVer
}

Export-ModuleMember -Function 'SyncGalleryToAzArtifacts'
