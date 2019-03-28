// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.

using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Management.Automation;
using System.Threading.Tasks;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace Microsoft.PowerShell
{
    /// <summary>
    /// A Helper class for printing notification on PowerShell startup when there is a new update.
    /// </summary>
    internal static class UpdatesNotification
    {
        private const string UpdateCheckOptOutEnvVar = "POWERSHELL_UPDATECHECK_OPTOUT";
        private const string Last4ReleasesUri = "https://api.github.com/repos/PowerShell/PowerShell/releases?per_page=4";
        private const string LatestReleaseUri = "https://api.github.com/repos/PowerShell/PowerShell/releases/latest";

        private const string SentinelFileName = "_sentinel_";
        private const string DoneFileNameTemplate = "sentinel-{0}-{1}-{2}.done";
        private const string DoneFileNamePattern = "sentinel-*.done";
        private const string UpdateFileNameTemplate = "update_{0}_{1}-{2}-{3}";
        private const string UpdateFileNamePattern = "update_v*.*.*_????-??-??";

        internal readonly static EnumerationOptions EnumerationOptions = new EnumerationOptions();

        internal static async Task CheckForUpdates()
        {
            // Delay the update check for 3 seconds so that it has the minimal impact on startup.
            await Task.Delay(3000);

            // A self-built pwsh for development purpose has the SHA1 commit hash baked in 'GitCommitId',
            // which is 40 characters long. So we can quickly check the length of 'GitCommitId' to tell
            // if this is a self-built pwsh, and skip the update check if so.
            if (PSVersionInfo.GitCommitId.Length > 40)
            {
                return;
            }

            // If the update check is opt out, then skip the rest of the check.
            if (Utils.GetOptOutEnvironmentVariableAsBool(UpdateCheckOptOutEnvVar, defaultValue: false))
            {
                return;
            }

#if UNIX
            string cacheDirectory = Path.Combine(
                Platform.SelectProductNameForDirectory(Platform.XDG_Type.CACHE),
                PSVersionInfo.GitCommitId);
#else
            string cacheDirectory = Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
                @"Microsoft\PowerShell",
                PSVersionInfo.GitCommitId);
#endif

            // Create the update cache directory if it hasn't exists
            if (!Directory.Exists(cacheDirectory))
            {
                Directory.CreateDirectory(cacheDirectory);
            }

            DateTime today = DateTime.UtcNow;

            if (TryParseUpdateFile(
                    cacheDirectory,
                    out bool noFileFound,
                    updateFilePath: out _,
                    lastUpdateVersion: out _,
                    out DateTime lastUpdateDate))
            {
                if (!noFileFound && (today - lastUpdateDate).TotalDays < 7)
                {
                    // There is an existing update file, and the last update was release less than a week.
                    // It's unlikely a new version is release within a week, so we can skip this check.
                    return;
                }
            }

            // Construct the sentinel file paths for today's check.
            string todayDoneFileName = string.Format(
                CultureInfo.InvariantCulture,
                DoneFileNameTemplate,
                today.Year.ToString(),
                today.Month.ToString(),
                today.Day.ToString());

            string sentinelFilePath = Path.Combine(cacheDirectory, SentinelFileName);
            string todayDoneFilePath = Path.Combine(cacheDirectory, todayDoneFileName);

            if (File.Exists(todayDoneFilePath))
            {
                // A successful update check has been done today.
                // We can skip this update check.
                return;
            }

            try
            {
                // Use 'sentinelFilePath' as the file lock.
                // The update check tasks started by every 'pwsh' process will compete on holding this file.
                using (FileStream s = new FileStream(sentinelFilePath, FileMode.Create, FileAccess.Write, FileShare.None))
                {
                    if (File.Exists(todayDoneFilePath))
                    {
                        // After acquiring the file lock, it turns out a successful check has already been done for today.
                        // Then let's skip this update check.
                        return;
                    }

                    // Now it's guaranteed that I'm the only process that reaches here.
                    // Clean up the old '.done' file, there should be only one of it.
                    foreach (string oldFile in Directory.EnumerateFiles(cacheDirectory, DoneFileNamePattern, EnumerationOptions))
                    {
                        File.Delete(oldFile);
                    }

                    if (!TryParseUpdateFile(
                            cacheDirectory,
                            noFileFound: out _,
                            out string updateFilePath,
                            out SemanticVersion lastUpdateVersion,
                            lastUpdateDate: out _))
                    {
                        // The update file is corrupted, either because more than one update files were found unexpectedly,
                        // or because the update file name is not in the valid format.
                        // This is **very unlikely** to happen unless the file is altered manually accidentally.
                        // We try to recover here by cleaning up all update files.
                        foreach (string file in Directory.EnumerateFiles(cacheDirectory, UpdateFileNamePattern, EnumerationOptions))
                        {
                            File.Delete(file);
                        }
                    }

                    // Do the real update check:
                    //  - Send HTTP request to query for the new release/pre-release;
                    //  - If there is a valid new release that should be reported to the user,
                    //    create the file `update_<tag>_<publish-date>` when no `update` file exists,
                    //    or rename the existing file to `update_<new-version>_<new-publish-date>`.
                    SemanticVersion baselineVersion = lastUpdateVersion ?? PSVersionInfo.PSV6Version;
                    Release release = await QueryNewReleaseAsync(baselineVersion);

                    if (release != null)
                    {
                        string newUpdateFileName = string.Format(
                            CultureInfo.InvariantCulture,
                            UpdateFileNameTemplate,
                            release.tag_name,
                            release.published_at.Year.ToString(),
                            release.published_at.Month.ToString(),
                            release.published_at.Day.ToString());

                        string newUpdateFilePath = Path.Combine(cacheDirectory, newUpdateFileName);

                        if (updateFilePath == null)
                        {
                            new FileStream(newUpdateFilePath, FileMode.CreateNew, FileAccess.Write, FileShare.None).Close();
                        }
                        else
                        {
                            File.Move(updateFilePath, newUpdateFilePath);
                        }
                    }

                    // Finally, create the `todayDoneFilePath` file as an indicator that a successful update check has finished today.
                    new FileStream(todayDoneFilePath, FileMode.CreateNew, FileAccess.Write, FileShare.None).Close();
                }
            }
            catch (Exception)
            {
                // An update check initiated from another `pwsh` process is in progress.
                // It's OK to just return and let that update check to finish the work.
            }
        }

        private static bool TryParseUpdateFile(
            string cacheDirectory,
            out bool noFileFound,
            out string updateFilePath,
            out SemanticVersion lastUpdateVersion,
            out DateTime lastUpdateDate)
        {
            noFileFound = false;
            updateFilePath = null;
            lastUpdateVersion = null;
            lastUpdateDate = DateTime.MinValue;

            var files = Directory.EnumerateFiles(cacheDirectory, UpdateFileNamePattern, EnumerationOptions);
            var enumerator = files.GetEnumerator();

            if (!enumerator.MoveNext())
            {
                // No file was found that matches the pattern, but it's OK that an update file doesn't exist.
                // This could happen when there is no new updates yet.
                noFileFound = true;
                return true;
            }

            updateFilePath = enumerator.Current;
            if (enumerator.MoveNext())
            {
                // More than 1 files were found that match the pattern. This is a corrupted state. 
                // Theoretically, there should be only one update file at any point of time.
                updateFilePath = null;
                return false;
            }

            // OK, only found one update file, which is expected.
            // Now let's parse the file name.
            string updateFileName = Path.GetFileName(updateFilePath);
            int dateStartIndex = updateFileName.LastIndexOf('_') + 1;

            bool success = DateTime.TryParse(
                updateFileName.AsSpan().Slice(dateStartIndex),
                CultureInfo.InvariantCulture,
                DateTimeStyles.None,
                out lastUpdateDate);

            if (success)
            {
                int versionStartIndex = updateFileName.IndexOf('_') + 2;
                int versionLength = dateStartIndex - versionStartIndex - 1;
                string versionString = updateFileName.Substring(versionStartIndex, versionLength);
                success = SemanticVersion.TryParse(versionString, out lastUpdateVersion);
            }

            if (!success)
            {
                updateFilePath = null;
                lastUpdateVersion = null;
                lastUpdateDate = DateTime.MinValue;
            }

            return success;
        }

        private static async Task<Release> QueryNewReleaseAsync(SemanticVersion baselineVersion)
        {
            bool noPreRelease = string.IsNullOrEmpty(PSVersionInfo.PSV6Version.PreReleaseLabel);
            string queryUri = noPreRelease ? LatestReleaseUri : Last4ReleasesUri;

            using (HttpClient client = new HttpClient())
            {
                string userAgent = string.Format(CultureInfo.InvariantCulture, "PowerShell {0}", PSVersionInfo.GitCommitId);
                client.DefaultRequestHeaders.Add("User-Agent", userAgent);
                client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));

                var response = await client.GetAsync(queryUri);
                response.EnsureSuccessStatusCode();

                using (var stream = await response.Content.ReadAsStreamAsync())
                using (var reader = new StreamReader(stream))
                using (JsonReader jsonReader = new JsonTextReader(reader))
                {
                    Release releaseToReturn = null;
                    JsonSerializer serializer = new JsonSerializer();

                    if (noPreRelease)
                    {
                        var latestRelease = serializer.Deserialize<Release>(jsonReader);
                        var version = SemanticVersion.Parse(latestRelease.tag_name.Substring(1));

                        if (version > baselineVersion)
                        {
                            releaseToReturn = latestRelease;
                        }
                    }
                    else
                    {
                        var last4Releases = serializer.Deserialize<List<Release>>(jsonReader);
                        var highestVersion = baselineVersion;

                        foreach (Release release in last4Releases)
                        {
                            var version = SemanticVersion.Parse(release.tag_name.Substring(1));
                            if (version > highestVersion)
                            {
                                highestVersion = version;
                                releaseToReturn = release;
                            }
                        }
                    }

                    return releaseToReturn;
                }
            }
        }

        private class Release
        {
            public DateTime published_at { get; set; }
            public string tag_name { get; set; }
            public bool prerelease { get; set; }
        }
    }
}
