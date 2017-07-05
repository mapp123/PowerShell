//
//    Copyright (C) Microsoft.  All rights reserved.
//

using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;

namespace System.Management.Automation
{
    internal static class ExtensionMethods
    {
        public static void SafeInvoke(this EventHandler eventHandler, object sender, EventArgs eventArgs)
        {
            if (eventHandler != null)
            {
                eventHandler(sender, eventArgs);
            }
        }

        public static void SafeInvoke<T>(this EventHandler<T> eventHandler, object sender, T eventArgs) where T : EventArgs
        {
            if (eventHandler != null)
            {
                eventHandler(sender, eventArgs);
            }
        }
    }

    internal static class EnumerableExtensions
    {
        internal static IEnumerable<T> Prepend<T>(this IEnumerable<T> collection, T element)
        {
            yield return element;
            foreach (T t in collection)
                yield return t;
        }

        internal static int SequenceGetHashCode<T>(this IEnumerable<T> xs) where T : class
        {
            // algorithm based on http://stackoverflow.com/questions/263400/what-is-the-best-algorithm-for-an-overridden-system-object-gethashcode
            if (xs == null)
            {
                return 82460653; // random number
            }
            unchecked
            {
                int hash = 41; // 41 is a random prime number
                foreach (T x in xs)
                {
                    hash = hash * 59; // 59 is a random prime number
                    if (x != null)
                    {
                        hash = hash + x.GetHashCode();
                    }
                }
                return hash;
            }
        }
    }

    /// <summary>
    /// The type extension methods within this partial class are used/shared by both FullCLR and CoreCLR powershell.
    ///
    /// * If you want to add an extension method that will be used by both FullCLR and CoreCLR powershell, please add it here.
    /// * If you want to add an extension method that will be used only by CoreCLR powershell, please add it to the partial
    ///   'PSTypeExtensions' class in 'CorePsExtensions.cs'.
    /// </summary>
    internal static partial class PSTypeExtensions
    {
        /// <summary>
        /// Type.EmptyTypes is not in CoreCLR. Use this one to replace it.
        /// </summary>
        internal static Type[] EmptyTypes = new Type[0];

        /// <summary>
        /// Check does the type have an instance default constructor with visibility that allows calling it from subclass.
        /// </summary>
        /// <param name="type">type</param>
        /// <returns>true when type has a default ctor.</returns>
        internal static bool HasDefaultCtor(this Type type)
        {
            var ctor = type.GetConstructor(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic, null, Type.EmptyTypes, null);
            if (ctor != null)
            {
                if (ctor.IsPublic || ctor.IsFamily || ctor.IsFamilyOrAssembly)
                {
                    return true;
                }
            }

            return false;
        }

        internal static bool IsNumeric(this Type type)
        {
            return LanguagePrimitives.IsNumeric(LanguagePrimitives.GetTypeCode(type));
        }

        internal static bool IsNumericOrPrimitive(this Type type)
        {
            return type.GetTypeInfo().IsPrimitive || LanguagePrimitives.IsNumeric(LanguagePrimitives.GetTypeCode(type));
        }

        internal static bool IsSafePrimitive(this Type type)
        {
            return type.GetTypeInfo().IsPrimitive && (type != typeof(IntPtr)) && (type != typeof(UIntPtr));
        }

        internal static bool IsFloating(this Type type)
        {
            return LanguagePrimitives.IsFloating(LanguagePrimitives.GetTypeCode(type));
        }

        internal static bool IsInteger(this Type type)
        {
            return LanguagePrimitives.IsInteger(LanguagePrimitives.GetTypeCode(type));
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        internal static TypeCode GetTypeCode(this Type type)
        {
            return Type.GetTypeCode(type);
        }

        internal static IEnumerable<T> GetCustomAttributes<T>(this Type type, bool inherit)
            where T : Attribute
        {
            return from attr in type.GetTypeInfo().GetCustomAttributes(typeof(T), inherit)
                   where attr is T
                   select (T)attr;
        }
    }

    internal static class WeakReferenceExtensions
    {
        internal static bool TryGetTarget<T>(this WeakReference weakReference, out T target) where T : class
        {
            var t = weakReference.Target;
            target = t as T;
            return (target != null);
        }
    }

    /// <summary>
    /// Extension methods for MetadataReader.
    /// </summary>
    internal static class MetadataReaderExtensions
    {
        // Format strings for constructing type names
        private const string Format_RegularType = "{0}.{1}";
        private const string Format_NestedType = "{0}+{1}";

        /// <summary>
        /// Get the name of TypeDefinition without considering nested type.
        /// </summary>
        private static string GetTypeSimpleName(MetadataReader reader, TypeDefinition typeDef)
            => typeDef.Namespace.IsNil 
                    ? reader.GetString(typeDef.Name)
                    : string.Format(CultureInfo.InvariantCulture, Format_RegularType,
                                    reader.GetString(typeDef.Namespace),
                                    reader.GetString(typeDef.Name));
        
        /// <summary>
        /// Get the name of TypeReference without considering nested type.
        /// </summary>
        private static string GetTypeSimpleName(MetadataReader reader, TypeReference typeRef)
            => typeRef.Namespace.IsNil 
                    ? reader.GetString(typeRef.Name)
                    : string.Format(CultureInfo.InvariantCulture, Format_RegularType,
                                    reader.GetString(typeRef.Namespace),
                                    reader.GetString(typeRef.Name));

        /// <summary>
        /// Get the FullName of a type definition.
        /// </summary>
        internal static string GetTypeFullName(this MetadataReader reader, TypeDefinition typeDef)
        {
            string fullName;

            // Get the enclosing type if the type is nested
            TypeDefinitionHandle declaringTypeHandle = typeDef.GetDeclaringType();
            if (declaringTypeHandle.IsNil)
            {
                fullName = GetTypeSimpleName(reader, typeDef);
            }
            else
            {
                fullName = reader.GetString(typeDef.Name);
                while (!declaringTypeHandle.IsNil)
                {
                    TypeDefinition declaringTypeDef = reader.GetTypeDefinition(declaringTypeHandle);
                    declaringTypeHandle = declaringTypeDef.GetDeclaringType();
                    if (declaringTypeHandle.IsNil)
                    {
                        fullName = string.Format(CultureInfo.InvariantCulture, Format_NestedType,
                                                 GetTypeSimpleName(reader, declaringTypeDef),
                                                 fullName);
                    }
                    else
                    {
                        fullName = string.Format(CultureInfo.InvariantCulture, Format_NestedType,
                                                 reader.GetString(declaringTypeDef.Name),
                                                 fullName);
                    }
                }
            }

            return fullName;
        }

        /// <summary>
        /// Get the FullName of a type reference.
        /// </summary>
        internal static string GetTypeFullName(this MetadataReader reader, TypeReference typeRef)
        {
            string fullName;

            EntityHandle resolutionScope = typeRef.ResolutionScope;
            if (resolutionScope.IsNil || resolutionScope.Kind != HandleKind.TypeReference)
            {
                fullName = GetTypeSimpleName(reader, typeRef);
            }
            else
            {
                // It's a nested type.
                // According to ECMA-335, if this is a nested type, then 'ResolutionScope' is a TypeRef token
                fullName = reader.GetString(typeRef.Name);
                while (!resolutionScope.IsNil && resolutionScope.Kind == HandleKind.TypeReference)
                {
                    TypeReference declaringTypeRef = reader.GetTypeReference((TypeReferenceHandle)resolutionScope);
                    resolutionScope = declaringTypeRef.ResolutionScope;
                    if (resolutionScope.IsNil || resolutionScope.Kind != HandleKind.TypeReference)
                    {
                        fullName = string.Format(CultureInfo.InvariantCulture, Format_NestedType,
                                                 GetTypeSimpleName(reader, declaringTypeRef),
                                                 fullName);
                    }
                    else
                    {
                        fullName = string.Format(CultureInfo.InvariantCulture, Format_NestedType,
                                                 reader.GetString(declaringTypeRef.Name),
                                                 fullName);
                    }
                }
            }

            return fullName;
        }

        /// <summary>
        /// Get the StrongName of an assembly.
        /// </summary>
        internal static string GetAssemblyStrongName(this MetadataReader reader)
        {
            AssemblyDefinition assemblyDefinition = reader.GetAssemblyDefinition();
            string asmName = reader.GetString(assemblyDefinition.Name);
            string asmVersion = assemblyDefinition.Version.ToString();
            string asmCulture = reader.GetString(assemblyDefinition.Culture);
            asmCulture = (asmCulture == string.Empty) ? "neutral" : asmCulture;

            AssemblyHashAlgorithm hashAlgorithm = assemblyDefinition.HashAlgorithm;
            BlobHandle blobHandle = assemblyDefinition.PublicKey;
            BlobReader blobReader = reader.GetBlobReader(blobHandle);
            byte[] publickey = blobReader.ReadBytes(blobReader.Length);

            HashAlgorithm hashImpl = null;
            switch (hashAlgorithm)
            {
                case AssemblyHashAlgorithm.Sha1:
                    hashImpl = SHA1.Create();
                    break;
                case AssemblyHashAlgorithm.MD5:
                    hashImpl = MD5.Create();
                    break;
                case AssemblyHashAlgorithm.Sha256:
                    hashImpl = SHA256.Create();
                    break;
                case AssemblyHashAlgorithm.Sha384:
                    hashImpl = SHA384.Create();
                    break;
                case AssemblyHashAlgorithm.Sha512:
                    hashImpl = SHA512.Create();
                    break;
                default:
                    throw new NotSupportedException();
            }

            byte[] publicKeyHash = hashImpl.ComputeHash(publickey);
            byte[] publicKeyTokenBytes = new byte[8];
            // Note that, the low 8 bytes of the hash of public key in reverse order is the public key tokens.
            for (int i = 1; i <= 8; i++)
            {
                publicKeyTokenBytes[i - 1] = publicKeyHash[publicKeyHash.Length - i];
            }

            // Convert bytes to hex format strings in lower case.
            string publicKeyTokenString = BitConverter.ToString(publicKeyTokenBytes).Replace("-", string.Empty).ToLowerInvariant();
            string strongAssemblyName = string.Format(CultureInfo.InvariantCulture,
                                                      "{0}, Version={1}, Culture={2}, PublicKeyToken={3}",
                                                      asmName, asmVersion, asmCulture, publicKeyTokenString);

            return strongAssemblyName;
        }

        /// <summary>
        /// Check if the attribute type is expected.
        /// </summary>
        internal static bool IsAttributeOfType(this MetadataReader reader, CustomAttribute customAttribute, Type attributeType)
        {
            return IsAttributeOfType(reader, customAttribute, attributeType.FullName);
        }

        /// <summary>
        /// Check if the attribute type is expected.
        /// </summary>
        internal static bool IsAttributeOfType(this MetadataReader reader, CustomAttribute customAttribute, string expectedTypeName)
        {
            string attributeFullName = null;
            switch (customAttribute.Constructor.Kind)
            {
                case HandleKind.MethodDefinition:
                    // Attribute is defined in the same module
                    MethodDefinition methodDef = reader.GetMethodDefinition((MethodDefinitionHandle)customAttribute.Constructor);
                    TypeDefinitionHandle declaringTypeDefHandle = methodDef.GetDeclaringType();
                    if (declaringTypeDefHandle.IsNil) { /* Global method */ return false; }

                    TypeDefinition declaringTypeDef = reader.GetTypeDefinition(declaringTypeDefHandle);
                    attributeFullName = GetTypeFullName(reader, declaringTypeDef);
                    break;

                case HandleKind.MemberReference:
                    MemberReference memberRef = reader.GetMemberReference((MemberReferenceHandle)customAttribute.Constructor);
                    switch (memberRef.Parent.Kind)
                    {
                        case HandleKind.TypeReference:
                            TypeReference typeRef = reader.GetTypeReference((TypeReferenceHandle)memberRef.Parent);
                            attributeFullName = GetTypeFullName(reader, typeRef);
                            break;

                        case HandleKind.TypeDefinition:
                            TypeDefinition typeDef = reader.GetTypeDefinition((TypeDefinitionHandle)memberRef.Parent);
                            attributeFullName = GetTypeFullName(reader, typeDef);
                            break;

                        default:
                            // constructor is global method, vararg method, or from a generic type.
                            return false;
                    }
                    break;

                default:
                    throw new BadImageFormatException("Invalid custom attribute.");
            }

            return string.Equals(attributeFullName, expectedTypeName, StringComparison.Ordinal);
        }

        /// <summary>
        /// Check if the type definition is an Enum.
        /// </summary>
        internal static bool IsTypeEnum(this MetadataReader reader, TypeDefinition typeDef)
        {
            if (!typeDef.IsSealed() || typeDef.BaseType.IsNil)
            {
                // Enum should be sealed and derived from System.Enum.
                return false;
            }

            // Enum is value type, so they are sealed. We only need to look at the direct base type.
            switch (typeDef.BaseType.Kind)
            {
                case HandleKind.TypeReference:
                    TypeReference baseType = reader.GetTypeReference((TypeReferenceHandle)typeDef.BaseType);
                    string baseTypeName = reader.GetTypeFullName(baseType);
                    return String.Equals(baseTypeName, "System.Enum", StringComparison.Ordinal);

                default:
                    // Kind == TypeDefinition:    BaseType is from the same assembly, so it shouldn't be 'System.Enum'.
                    // Kind == TypeSpecification: BaseType is a specialized generic type, such as List<String>.
                    return false; 
            }
        }

        /// <summary>
        /// Get member names of an Enum type.
        /// </summary>
        internal static List<string> GetEnumNames(this MetadataReader reader, TypeDefinition typeDef)
        {
            const FieldAttributes literalAndStatic = FieldAttributes.Literal | FieldAttributes.Static;
            if (!reader.IsTypeEnum(typeDef))
            {
                return null;
            }

            List<string> names = new List<string>();
            foreach (FieldDefinitionHandle fieldHandle in typeDef.GetFields())
            {
                FieldDefinition fieldDef = reader.GetFieldDefinition(fieldHandle);
                // Enum members are the static and literal fields.
                if ((fieldDef.Attributes & literalAndStatic) == literalAndStatic)
                {
                    names.Add(reader.GetString(fieldDef.Name));
                }
            }

            return names;
        }
    }

    /// <summary>
    /// Extension methods for TypeDefinition.
    /// </summary>
    internal static class TypeDefinitionExtensions
    {
        /// <summary>
        /// Check if the type definition is generic.
        /// </summary>
        internal static bool IsGeneric(this TypeDefinition typeDef)
        {
            return typeDef.GetGenericParameters().Count > 0;
        }

        /// <summary>
        /// Check if the type definition is public.
        /// </summary>
        internal static bool IsPublic(this TypeDefinition typeDef)
        {
            // The visibility mask is used to mask out the bits that contain the visibility.
            // The visibilities are not combineable, e.g. you can't be both public and private, which is why these aren't independent powers of two.
            TypeAttributes visibilityBits = typeDef.Attributes & TypeAttributes.VisibilityMask;
            return visibilityBits == TypeAttributes.Public || visibilityBits == TypeAttributes.NestedPublic;
        }

        /// <summary>
        /// Check if the type definition is nested.
        /// </summary>
        internal static bool IsNested(this TypeDefinition typeDef)
        {
            // Whether a type is nested can be determined by the value of its Flags.Visibility sub-field
            // - it shall be one of the set:
            // {NestedPublic:2, NestedPrivate:3, NestedFamily:4, NestedAssembly:5, NestedFamANDAssem:6, NestedFamORAssem:7}
            TypeAttributes visibilityBits = typeDef.Attributes & TypeAttributes.VisibilityMask;
            int bitsValue = (int) visibilityBits;
            return bitsValue >= 2 && bitsValue <= 7;
        }

        /// <summary>
        /// Check if the type definition is abstract.
        /// </summary>
        internal static bool IsAbstract(this TypeDefinition typeDef)
        {
            // An interface is always abstract.
            return (typeDef.Attributes & TypeAttributes.Abstract) == TypeAttributes.Abstract;
        }

        /// <summary>
        /// Check if the type definition is sealed.
        /// </summary>
        internal static bool IsSealed(this TypeDefinition typeDef)
        {
            return (typeDef.Attributes & TypeAttributes.Sealed) == TypeAttributes.Sealed;
        }

        /// <summary>
        /// Check if the type definition is an Interface.
        /// </summary>
        internal static bool IsInterface(this TypeDefinition typeDef)
        {
            TypeAttributes classSemanticBits = typeDef.Attributes & TypeAttributes.ClassSemanticsMask;
            return classSemanticBits == TypeAttributes.Interface;
        }
    }
}
