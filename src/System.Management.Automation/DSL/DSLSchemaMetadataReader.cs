/********************************************************************++
Copyright (c) Microsoft Corporation.  All rights reserved.
--********************************************************************/

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Management.Automation;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using System.Text;

using MethodAttributes = System.Reflection.MethodAttributes;

namespace System.Management.Automation.Language
{
    /// <summary>
    /// The base symbol type provider.
    /// </summary>
    internal abstract class BasicSymbolTypeProvider : ISimpleTypeProvider<Type>, ISZArrayTypeProvider<Type>
    {
        /// <summary>
        /// Gets the type for a single-dimensional array with zero lower bounds of the given element type.
        /// </summary>
        public virtual Type GetSZArrayType(Type elementType)
            => elementType.MakeArrayType();

        /// <summary>
        /// Get the type for a primitive type.
        /// </summary>
        public virtual Type GetPrimitiveType(PrimitiveTypeCode typeCode)
        {
            switch (typeCode)
            {
                case PrimitiveTypeCode.Boolean:
                    return typeof(bool);

                case PrimitiveTypeCode.Byte:
                    return typeof(byte);

                case PrimitiveTypeCode.Char:
                    return typeof(char);

                case PrimitiveTypeCode.Double:
                    return typeof(double);

                case PrimitiveTypeCode.Int16:
                    return typeof(short);

                case PrimitiveTypeCode.Int32:
                    return typeof(int);

                case PrimitiveTypeCode.Int64:
                    return typeof(long);

                case PrimitiveTypeCode.IntPtr:
                    return typeof(IntPtr);

                case PrimitiveTypeCode.Object:
                    return typeof(object);

                case PrimitiveTypeCode.SByte:
                    return typeof(sbyte);

                case PrimitiveTypeCode.Single:
                    return typeof(float);

                case PrimitiveTypeCode.String:
                    return typeof(string);

                case PrimitiveTypeCode.TypedReference:
                    return typeof(TypedReference);

                case PrimitiveTypeCode.UInt16:
                    return typeof(ushort);

                case PrimitiveTypeCode.UInt32:
                    return typeof(uint);

                case PrimitiveTypeCode.UInt64:
                    return typeof(ulong);

                case PrimitiveTypeCode.UIntPtr:
                    return typeof(UIntPtr);

                case PrimitiveTypeCode.Void:
                    return typeof(void);

                default:
                    throw new ArgumentOutOfRangeException(nameof(typeCode));
            }
        }

        /// <summary>
        /// Get the type symbol of a type definition.
        /// </summary>
        public virtual Type GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind)
            => throw new NotImplementedException(nameof(GetTypeFromDefinition));
        
        /// <summary>
        /// Get the type symbol of a type reference.
        /// </summary>
        public virtual Type GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind)
            => throw new NotImplementedException(nameof(GetTypeFromReference));
    }

    /// <summary>
    /// The base symbol name provider.
    /// </summary>
    internal abstract class BasicSymbolNameProvider : ISimpleTypeProvider<string>, ISZArrayTypeProvider<string>
    {
        /// <summary>
        /// Get a string representation for a single-dimensional array with zero lower bounds of the given element type.
        /// </summary>
        /// <param name="elementType">the base type of elements in the array</param>
        /// <returns>the string representing an SZArray of elements of type <paramref name="elementType"/></returns>
        public virtual string GetSZArrayType(string elementType)
            => elementType + "[]";
        
        /// <summary>
        /// Get a string representation for a primitive type.
        /// </summary>
        /// <param name="typeCode">the metadata type code representing the given primitive type</param>
        /// <returns>the short form C# string representation of the given primitive type</returns>
        public virtual string GetPrimitiveType(PrimitiveTypeCode typeCode)
            => "System." + typeCode.ToString();

        /// <summary>
        /// Get a string representation for a type definition.
        /// </summary>
        /// <param name="reader">The metadata reader that was passed to the signature decoder. It may be null.</param>
        /// <param name="handle">The type definition handle.</param>
        /// <param name="rawTypeKind">The kind of the type as specified in the signature.</param>
        public virtual string GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind)
            => reader?.GetTypeFullName(reader.GetTypeDefinition(handle));

        /// <summary>
        /// Get a string representation for a type reference.
        /// </summary>
        /// <param name="reader">The metadata reader that was passed to the signature decoder. It may be null.</param>
        /// <param name="handle">The type reference handle.</param>
        /// <param name="rawTypeKind">The kind of the type as specified in the signature.</param>
        public virtual string GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind)
            => reader?.GetTypeFullName(reader.GetTypeReference(handle));
    }

    /// <summary>
    /// A CustomAttribute symbol type provider to help decode the custom attributes that only reference PowerShell built-in types.
    /// </summary>
    /// <remarks>
    /// This provider tries to resolve types. It may not work for an arbitrary attribute, such as an attribute references to unloaded types.
    /// </remarks>
    internal sealed class AttributeTypeProvider : BasicSymbolTypeProvider, ICustomAttributeTypeProvider<Type>
    {
        /// <summary>
        /// Get the Type representation of <see cref="System.Type"/>.
        /// </summary>
        public Type GetSystemType()
            => typeof(Type);
        
        /// <summary>
        /// Check whether a type is <see cref="System.Type"/>.
        /// </summary>
        public bool IsSystemType(Type type)
            => type == typeof(Type);

        /// <summary>
        /// Get the type for the given serialized type name.
        /// </summary>
        public Type GetTypeFromSerializedName(string name)
            => LanguagePrimitives.ConvertTo<Type>(name);

        /// <summary>
        /// Get the underlying type of an enum type.
        /// </summary>
        public PrimitiveTypeCode GetUnderlyingEnumType(Type type)
        {
            if (type.IsEnum)
            {
                // Underlying type of an enum type should be one of the following types:
                // - byte, sbyte, short, ushort, int, uint, long, or ulong
                TypeCode typeCode = Type.GetTypeCode(Enum.GetUnderlyingType(type));
                switch (typeCode)
                {
                    case TypeCode.Byte:
                        return PrimitiveTypeCode.Byte;
                    case TypeCode.SByte:
                        return PrimitiveTypeCode.SByte;
                    case TypeCode.Int16:
                        return PrimitiveTypeCode.Int16;
                    case TypeCode.UInt16:
                        return PrimitiveTypeCode.UInt16;
                    case TypeCode.Int32:
                        return PrimitiveTypeCode.Int32;
                    case TypeCode.UInt32:
                        return PrimitiveTypeCode.UInt32;
                    case TypeCode.Int64:
                        return PrimitiveTypeCode.Int64;
                    case TypeCode.UInt64:
                        return PrimitiveTypeCode.UInt64;
                    
                    default: throw new ArgumentOutOfRangeException(nameof(type));
                }
            }

            throw new ArgumentException(nameof(type));
        }
    }

    /// <summary>
    /// A CustomAttribute symbol name provider to help decode the custom attributes that may reference unloaded types.
    /// </summary>
    /// <remarks>
    /// This provider represents type symbols in string format. It may not work for an arbitrary attribute.
    /// </remarks>
    internal sealed class AttributeNameProvider : BasicSymbolNameProvider, ICustomAttributeTypeProvider<string>
    {
        /// <summary>
        /// Get the type symbol representation of <see cref="System.Type"/>.
        /// </summary>
        public string GetSystemType()
            => typeof(Type).FullName;
        
        /// <summary>
        /// Check whether a type symbol is <see cref="System.Type"/>.
        /// </summary>
        public bool IsSystemType(string type)
            => string.Equals(type, typeof(Type).FullName, StringComparison.Ordinal);

        /// <summary>
        /// Get the type symbol for the given serialized type name.
        /// </summary>
        public string GetTypeFromSerializedName(string name)
            => name;

        /// <summary>
        /// Get the underlying type symbol of an enum type symbol.
        /// </summary>
        public PrimitiveTypeCode GetUnderlyingEnumType(string type)
            => PrimitiveTypeCode.Int32; // Assume the underlying type of an enum is always Int32
    }

    /// <summary>
    /// A metadata type interpreter that renders metadata types as strings.
    /// </summary>
    internal sealed class StringSignatureProvider : BasicSymbolNameProvider, ISignatureTypeProvider<string, object>
    {
        /// <summary>
        /// Get a string representation for a generic instantiation of the given generic type with the given type arguments.
        /// </summary>
        /// <param name="genericType">the representation of the generic type being instantiated</param>
        /// <param name="typeArguments">string representations of the type parameters of the generic type</param>
        public string GetGenericInstantiation(string genericType, ImmutableArray<string> typeArguments)
            => genericType + "<" + String.Join(",", typeArguments) + ">";

        /// <summary>
        /// Get a string representation for a generalized array of the given element type and shape.
        /// </summary>
        /// <param name="elementType">the string representation of the base type of the array</param>
        /// <param name="shape">the shape of the array</param>
        public string GetArrayType(string elementType, ArrayShape shape)
        {
            var builder = new StringBuilder();

            builder.Append(elementType);
            builder.Append('[');

            for (int i = 0; i < shape.Rank; i++)
            {
                int lowerBound = 0;

                if (i < shape.LowerBounds.Length)
                {
                    lowerBound = shape.LowerBounds[i];
                    builder.Append(lowerBound);
                }

                builder.Append("...");

                if (i < shape.Sizes.Length)
                {
                    builder.Append(lowerBound + shape.Sizes[i] - 1);
                }

                if (i < shape.Rank - 1)
                {
                    builder.Append(',');
                }
            }

            builder.Append(']');
            return builder.ToString();
        }

        /// <summary>
        /// Get a string representation for a managed pointer to the given element type.
        /// </summary>
        /// <param name="elementType">the base type of the referenced element</param>
        /// <returns>the string representation of a reference to the element type</returns>
        public string GetByReferenceType(string elementType)
            => elementType + "&";

        /// <summary>
        /// Get a string representation for an unmanaged pointer to the given element type.
        /// </summary>
        /// <param name="elementType">string representing the base type being pointed to</param>
        /// <returns>the string representation of a pointer to the given type</returns>
        public string GetPointerType(string elementType)
            => elementType + "*";

        /// <summary>
        /// Get a string representation for the function pointer type of the given method signature.
        /// </summary>
        /// <param name="signature">the signature of the method being pointed to</param>
        public string GetFunctionPointerType(MethodSignature<string> signature)
        {
            ImmutableArray<string> parameterTypes = signature.ParameterTypes;

            int requiredParameterCount = signature.RequiredParameterCount;

            var builder = new StringBuilder();
            builder.Append("method ");
            builder.Append(signature.ReturnType);
            builder.Append(" *(");

            int i;
            for (i = 0; i < requiredParameterCount; i++)
            {
                builder.Append(parameterTypes[i]);
                if (i < parameterTypes.Length - 1)
                {
                    builder.Append(", ");
                }
            }

            if (i < parameterTypes.Length)
            {
                builder.Append("..., ");
                for (; i < parameterTypes.Length; i++)
                {
                    builder.Append(parameterTypes[i]);
                    if (i < parameterTypes.Length - 1)
                    {
                        builder.Append(", ");
                    }
                }
            }

            builder.Append(')');
            return builder.ToString();
        }

        /// <summary>
        /// Get a string representation for the generic method parameter at the given zero-based index.
        /// </summary>
        /// <param name="genericContext">the generic type context providing typing to the method signature type parameters</param>
        /// <param name="index">the index of the type parameter to represent</param>
        public string GetGenericMethodParameter(object genericContext, int index)
            => "!!" + index;

        /// <summary>
        /// Get a string representation for the generic type parameter at the given zero-based index.
        /// </summary>
        /// <param name="genericContext">the generic context in which this generic type is being instantiated</param>
        /// <param name="index">the index of the type parameter</param>
        public string GetGenericTypeParameter(object genericContext, int index)
            => "!" + index;

        /// <summary>
        /// Get a string representation for a type with a custom modifier applied.
        /// </summary>
        /// <param name="modifier">The modifier type applied.</param>
        /// <param name="unmodifiedType">The type symbol of the underlying type without modifiers applied.</param>
        /// <param name="isRequired">True if the modifier is required, false if it's optional.</param>
        public string GetModifiedType(string modifier, string unmodifiedType, bool isRequired)
            => (isRequired ? "modreq" : "modopt") + "(" + modifier + ") " + unmodifiedType;

        /// <summary>
        /// Get a string representation for a local variable type that is marked as pinned.
        /// </summary>
        public string GetPinnedType(string elementType)
            => "pinned " + elementType;

        /// <summary>
        /// Get a string representation for a type specification.
        /// </summary>
        /// <param name="reader">The metadata reader that was passed to the signature decoder. It may be null.</param>
        /// <param name="genericContext">The context that was passed to the signature decoder.</param>
        /// <param name="handle">The type specification handle.</param>
        /// <param name="rawTypeKind">The kind of the type as specified in the signature.</param>
        public string GetTypeFromSpecification(MetadataReader reader, object genericContext, TypeSpecificationHandle handle, byte rawTypeKind)
            => reader?.GetTypeSpecification(handle).DecodeSignature(this, genericContext);
    }

    /// <summary>
    /// Class to pass metadata reading errors back to the parser to be reported
    /// </summary>
    internal sealed class ParseErrorContainer
    {
        private string _errorId;
        private string _errorMessage;

        /// <summary>
        /// Construct a new ParseError value to pass up
        /// </summary>
        public ParseErrorContainer(string errorId, string errorStr, params object[] args)
        {
            _errorId = errorId;
            _errorMessage = String.Format(CultureInfo.CurrentCulture, errorStr, args);
        }

        /// <summary>
        /// Take the current parser extent to turn this ParseErrorContainer into a ParseError
        /// for the parser to report
        /// </summary>
        /// <param name="importExtent">the extent of the import statement that imports the module being parsed</param>
        /// <returns>a parser error expression the error to report</returns>
        public ParseError GenerateParseError(IScriptExtent importExtent)
        {
            return new ParseError(importExtent, _errorId, _errorMessage);
        }
    }

    /// <summary>
    /// Class to read DSL keywords from a given assembly.
    /// </summary>
    internal sealed class DSLKeywordMetadataReader
    {
    #region ModuleScope_Fields

        // The module we are processing
        private readonly PSModuleInfo _moduleInfo;

        // The string signature provider
        private readonly StringSignatureProvider _strSigProvider;

        // The attribute symbol name provider
        private readonly AttributeNameProvider _attrNameProvider;

        // List of parsing errors for the whole module
        private readonly List<ParseErrorContainer> _parseErrors_ModuleScope;

        // A set of Keyword names defined in the module
        private readonly HashSet<string> _keywordNames_ModuleScope;

        // Map from Keyword name to the Keyword for the whole module
        private readonly Dictionary<string, DynamicKeyword> _keywordNameToKeywordMap_ModuleScope;

        // Map from assembly path to the list of keyword type names whose parsing actions need to be
        // populated after loading the assembly, for the whole module
        private readonly Dictionary<string, List<string>> _assemblyToKeywordTypeNames_ModuleScope;

    #endregion

    #region AssemblyScope_Fields

        // A set of Keyword type names defined in the assembly being processed
        private readonly HashSet<string> _keywordTypeNames_AssemblyScope;

        // Map from keyword name to the nested keyword type names for the assembly being processed
        private readonly Dictionary<string, HashSet<string>> _keywordToNestedKeywordsMap_AssemblyScope;

        // Map from the Enum type name to the Enum member names for the assembly being processed
        private readonly Dictionary<string, List<string>> _enumTypeNameToMembers_AssemblyScope;

        // The metadata reader of the assembly being processed
        private MetadataReader _reader;

        // Path of the assembly being processed
        private string _assemblyPath;
    
    #endregion

        /// <summary>
        /// Construct a new DLL reader from a loaded module, whose information is
        /// described by a PSModuleInfo object
        /// </summary>
        /// <param name="moduleInfo">the information object describing the module to read Dynamic Keyword specifications from</param>
        private DSLKeywordMetadataReader(PSModuleInfo moduleInfo)
        {
            _moduleInfo = moduleInfo;
            _attrNameProvider = new AttributeNameProvider();
            _strSigProvider = new StringSignatureProvider();
            _parseErrors_ModuleScope = new List<ParseErrorContainer>();

            // Keyword names are case-insensitive in PowerShell, while Type names and assembly paths are case-sensitive.
            // Module scope collections
            _keywordNames_ModuleScope = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            _keywordNameToKeywordMap_ModuleScope = new Dictionary<string, DynamicKeyword>(StringComparer.OrdinalIgnoreCase);
            _assemblyToKeywordTypeNames_ModuleScope   = new Dictionary<string, List<string>>(StringComparer.Ordinal);

            // Assembly scope collections
            _keywordTypeNames_AssemblyScope = new HashSet<string>(StringComparer.Ordinal);
            _keywordToNestedKeywordsMap_AssemblyScope = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);
            _enumTypeNameToMembers_AssemblyScope      = new Dictionary<string, List<string>>(StringComparer.Ordinal);
        }

        internal static IEnumerable<DynamicKeyword> ReadDslKeywords(PSModuleInfo moduleInfo, out List<ParseErrorContainer> errors)
        {
            var dslReader = new DSLKeywordMetadataReader(moduleInfo);
            return dslReader.ReadDslKeywords(out errors);
        }

        /// <summary>
        /// Parse all globally defined keywords in the keyword specification by recursive descent
        /// </summary>
        /// <remarks>
        /// For now we require each DSL assembly is self-contained -- it doesn't depend on types
        /// from another assembly except for the keyword attribute/base types.
        /// </remarks>
        private IEnumerable<DynamicKeyword> ReadDslKeywords(out List<ParseErrorContainer> errors)
        {
            errors = _parseErrors_ModuleScope;

            var moduleAssemblies = Directory.EnumerateFiles(_moduleInfo.ModuleBase, "*.dll", SearchOption.AllDirectories);
            foreach (string dslAssemblyPath in moduleAssemblies)
            {
                _assemblyPath = dslAssemblyPath;
                try
                {
                    using (FileStream stream = File.OpenRead(dslAssemblyPath))
                    using (var peReader = new PEReader(stream))
                    {
                        // Skip the assembly if it has no metadata
                        if (!peReader.HasMetadata) { continue; }

                        bool isDslSchemaAssembly = false;
                        _reader = peReader.GetMetadataReader();
                        var assemblyDef = _reader.GetAssemblyDefinition();
                        
                        foreach (var attributeHandle in assemblyDef.GetCustomAttributes())
                        {
                            var attributeDef = _reader.GetCustomAttribute(attributeHandle);
                            isDslSchemaAssembly = _reader.IsAttributeOfType(attributeDef, typeof(PowerShellDSLSchemaAttribute));
                            
                            if (isDslSchemaAssembly) { break; }
                        }

                        // Skip the assembly if it doesn't have 'PowerShellDSLSchemaAttribute' declared
                        if (isDslSchemaAssembly)
                        {
                            // Read DSL keywords from the assembly
                            ReadDslKeywordsFromAssembly();
                        }
                    }
                }
                catch (BadImageFormatException)
                {
                    // The DLL is an invalid PE file. Ignore this assembly.
                }
            }

            // Carry on post processing actions only if there was no parsing errors
            if (errors.Count == 0)
            {
                RunPostModuleProcessingAction();
            }

            // If there are errors, return null. Otherwise, return the dynamic keywords. 
            return errors.Count == 0 ? _keywordNameToKeywordMap_ModuleScope.Values : null;
        }

        /// <summary>
        /// Read in all keywords defined in the assembly that is being processed
        /// </summary>
        private void ReadDslKeywordsFromAssembly()
        {
            // Clear collections that hold metadata of a specific assembly
            _enumTypeNameToMembers_AssemblyScope.Clear();
            _keywordToNestedKeywordsMap_AssemblyScope.Clear();
            _keywordTypeNames_AssemblyScope.Clear();

            foreach (var typeDefHandle in _reader.TypeDefinitions)
            {
                TypeDefinition typeDef = _reader.GetTypeDefinition(typeDefHandle);
                if (!typeDef.IsPublic() || typeDef.IsAbstract() || typeDef.IsNested() || typeDef.IsGeneric()) 
                {
                    // Skip non-public, abstract, nested or generic types.
                    continue;
                }

                if (_reader.IsTypeEnum(typeDef))
                {
                    var enumName = _reader.GetTypeFullName(typeDef);
                    var enumMembers = _reader.GetEnumNames(typeDef);
                    _enumTypeNameToMembers_AssemblyScope.Add(enumName, enumMembers);

                    continue;
                }

                if (IsDslKeyword(typeDef, out CustomAttribute? keywordAttribute, out CustomAttribute? nestedKeywordsAttribute))
                {
                    HashSet<string> nestedKeywordTypeNames = null;
                    if (nestedKeywordsAttribute.HasValue)
                    {
                        // We know for sure there is only one fixed argument for 'NestedKeywordsAttribute', so 'attrValue.FixedArguments'
                        // should be of length 1, and the value should be a collection of 'CustomAttributeTypedArgument<string>'.
                        var attrValue = nestedKeywordsAttribute.Value.DecodeValue(_attrNameProvider);
                        var attrTypeArgs = attrValue.FixedArguments[0].Value as IEnumerable<CustomAttributeTypedArgument<string>>;
                        if (attrTypeArgs != null && attrTypeArgs.Any())
                        {
                            nestedKeywordTypeNames = new HashSet<string>(StringComparer.Ordinal);
                            foreach (var typeArg in attrTypeArgs)
                            {
                                var nestedKeywordTypeName = (string)typeArg.Value;
                                if (!nestedKeywordTypeNames.Add(nestedKeywordTypeName))
                                {
                                    // The type name is already present
                                    // DSL-TODO: report error about duplicate nested keyword type names
                                }
                            }
                        }
                    }

                    _keywordTypeNames_AssemblyScope.Add(_reader.GetTypeFullName(typeDef));
                    DynamicKeyword keyword = ReadDslKeyword(typeDef, keywordAttribute.Value, nestedKeywordTypeNames);
                    if (keyword != null)
                    {
                        _keywordNameToKeywordMap_ModuleScope.Add(keyword.Keyword, keyword);
                    }
                }
            }

            // Carry on post processing actions only if there was no parsing errors
            if (_parseErrors_ModuleScope.Count == 0)
            {
                RunPostAssemblyProcessingAction();
            }
        }

        /// <summary>
        /// Extra validation after processing all keywords from the assembly
        /// </summary>
        private void RunPostAssemblyProcessingAction()
        {
            // 1. Create all keyword vertices
            int keywordCounts = _keywordToNestedKeywordsMap_AssemblyScope.Count;
            var vertices = new Dictionary<string, KeywordVertex>(keywordCounts, StringComparer.OrdinalIgnoreCase);
            foreach (var key in _keywordToNestedKeywordsMap_AssemblyScope.Keys)
            {
                vertices.Add(key, new KeywordVertex(key));
            }

            // 2. Check if all declared nested Keywords are actually keywords from the same assembly and construct the graph
            var graph = new Dictionary<KeywordVertex, List<KeywordVertex>>(keywordCounts);
            foreach (var pair in _keywordToNestedKeywordsMap_AssemblyScope)
            {
                List<KeywordVertex> value = null;
                var parentkeyword = _keywordNameToKeywordMap_ModuleScope[pair.Key];

                if (pair.Value != null)
                {
                    foreach (var nestedKeywordTypeName in pair.Value)
                    {
                        if (!_keywordTypeNames_AssemblyScope.Contains(nestedKeywordTypeName))
                        {
                            // DSL-TODO: nested keyword is not a keyword defined in the same assembly.
                            //           report error.
                            continue;
                        }

                        int lastIndexOfDot = nestedKeywordTypeName.LastIndexOf('.');
                        string nestedKeywordName = nestedKeywordTypeName.Substring(lastIndexOfDot + 1);
                        var nestedKeyword = _keywordNameToKeywordMap_ModuleScope[nestedKeywordName];
                        nestedKeyword.IsNested = true;

                        // No point to check for loops if we had any errors
                        if (_parseErrors_ModuleScope.Count == 0)
                        {
                            if (value == null) { value = new List<KeywordVertex>(); }
                            value.Add(vertices[nestedKeywordName]);
                            parentkeyword.InnerKeywords.Add(nestedKeywordName, nestedKeyword);
                        }
                    }
                }

                if (_parseErrors_ModuleScope.Count == 0)
                {
                    graph.Add(vertices[pair.Key], value);
                }
            }

            // 3. Top-level keyword should always have UseMode to be OptionalMany. Validate it.
            foreach (var keywordName in _keywordToNestedKeywordsMap_AssemblyScope.Keys)
            {
                var keyword = _keywordNameToKeywordMap_ModuleScope[keywordName];
                if (keyword.IsNested) { continue; }

                if (keyword.UseMode != DynamicKeywordUseMode.OptionalMany)
                {
                    // DSL-TODO: report error.
                }
            }

            // 4. Check for nested keyword loops
            // This check makes sure no error happens in step 2
            if (graph.Count == keywordCounts)
            {
                List<List<string>> loops = KeywordVertex.GetNestedKeywordLoops(graph);
                if (loops.Count > 0)
                {
                    // DSL-TODO: there are loops. report error.
                }
            }
        }

        /// <summary>
        /// Load assembly as needed to populate the parsing actions
        /// </summary>
        private void RunPostModuleProcessingAction()
        {
            foreach (var pair in _assemblyToKeywordTypeNames_ModuleScope)
            {
                string assemblyFile = pair.Key;
                List<string> keywordTypeNames = pair.Value;

                var assembly = Assembly.LoadFrom(assemblyFile);
                foreach (string typeName in keywordTypeNames)
                {
                    try
                    {
                        var type = assembly.GetType(typeName, throwOnError: true, ignoreCase: false);
                        var instance = (Keyword)Activator.CreateInstance(type);
                        
                        int lastDotIndex = typeName.LastIndexOf('.');
                        var keywordName = typeName.Substring(lastDotIndex + 1);

                        var keyword = _keywordNameToKeywordMap_ModuleScope[keywordName];
                        keyword.PreParse = instance.PreParse;
                        keyword.PostParse = instance.PostParse;
                        keyword.SemanticCheck = instance.SemanticCheck;
                    }
                    catch (Exception)
                    {
                        // DSL-TODO: report error
                    }
                }
            }
        }

        /// <summary>
        /// Checks if a type definition is a DSL keyword.
        ///  - it derives from abstract type Keyword [required]
        ///  - it declares KeywordAttribute [required]
        ///  - it declares NestedKeywordsAttribute [optional]
        /// </summary>
        private bool IsDslKeyword(TypeDefinition typeDef, out CustomAttribute? keywordAttribute, out CustomAttribute? nestedKeywordsAttribute)
        {
            // First check the type defininition has the KeywordAttribute
            keywordAttribute = null;
            nestedKeywordsAttribute = null;

            bool isParseActionDefined;
            if (!IsKeywordClass(typeDef, out isParseActionDefined)) { return false; }

            // Extract the first 'KeywordAttribute' and 'NestedKeywordsAttribute' we find.
            foreach (CustomAttributeHandle attributeHandle in typeDef.GetCustomAttributes())
            {
                CustomAttribute attribute = _reader.GetCustomAttribute(attributeHandle);
                if (_reader.IsAttributeOfType(attribute, typeof(KeywordAttribute)) && !keywordAttribute.HasValue)
                {
                    keywordAttribute = attribute;
                }
                else if (_reader.IsAttributeOfType(attribute, typeof(NestedKeywordsAttribute)) && !nestedKeywordsAttribute.HasValue)
                {
                    nestedKeywordsAttribute = attribute;
                }

                if (keywordAttribute.HasValue && nestedKeywordsAttribute.HasValue) { break; }
            }

            // If it's a keyword, do further processing if any parsing actions are defined
            if (keywordAttribute.HasValue && isParseActionDefined)
            {
                string typeName = _reader.GetTypeFullName(typeDef);
                if (_assemblyToKeywordTypeNames_ModuleScope.TryGetValue(_assemblyPath, out List<string> list))
                {
                    list.Add(typeName);
                }
                else
                {
                    _assemblyToKeywordTypeNames_ModuleScope.Add(_assemblyPath, new List<string>() { typeName });
                }
            }

            // Return true if 'KeywordAttribute' is defined
            return keywordAttribute.HasValue;
        }

        /// <summary>
        /// Checks if a type definition is derived from abstract type 'Keyword'
        ///  - Base type chains to 'Keyword'
        /// Also check if any parsing actions are defined, which determines if we need to load the assembly.
        /// </summary>
        private bool IsKeywordClass(TypeDefinition typeDef, out bool isParseActionDefined)
        {
            isParseActionDefined = false;
            bool isKeywordClass = false;

            // Keyword class should have a public default constructor
            if (!HasPublicDefaultConstructor(typeDef)) { return isKeywordClass; }

            var derivationChain = new List<TypeDefinition>() { typeDef };
            var baseTypeHandle = typeDef.BaseType;

            // Base type is in the same assembly, then we keep walking the chain
            while (!baseTypeHandle.IsNil && baseTypeHandle.Kind == HandleKind.TypeDefinition)
            {
                var baseTypeDef = _reader.GetTypeDefinition((TypeDefinitionHandle)baseTypeHandle);
                derivationChain.Add(baseTypeDef);
                baseTypeHandle = baseTypeDef.BaseType;
            }

            // BaseType is now either 'TypeRef' or 'TypeSpec'.
            //  - 'TypeSpec' is specialized generic type, so it's not Keyword class.
            //  - 'TypeRef' may be the Keyword class, so check the full type name.
            if (!baseTypeHandle.IsNil && baseTypeHandle.Kind == HandleKind.TypeReference)
            {
                var baseTypeRef = _reader.GetTypeReference((TypeReferenceHandle)baseTypeHandle);
                var fullName = _reader.GetTypeFullName(baseTypeRef);
                isKeywordClass = typeof(Keyword).FullName.Equals(fullName, StringComparison.Ordinal);
            }

            if (isKeywordClass)
            {
                // Parsing actions may be defined in a base type in the same assembly so that the same actions
                // can be applied to multiple keywords. That's why we need to walk the chain.
                foreach (var type in derivationChain)
                {
                    foreach (var methodHandle in type.GetMethods())
                    {
                        var methodDef = _reader.GetMethodDefinition(methodHandle);
                        if ((methodDef.Attributes & MethodAttributes.MemberAccessMask) != MethodAttributes.Public ||
                            (methodDef.Attributes & MethodAttributes.Virtual) != MethodAttributes.Virtual ||
                            methodDef.GetGenericParameters().Count > 0)
                        {
                            continue; // The method is non-public, or not virtual, or is generic
                        }

                        string name = _reader.GetString(methodDef.Name);
                        string paramTypeName = null;
                        switch (name)
                        {
                            case nameof(Keyword.PreParse):
                                paramTypeName = typeof(DynamicKeyword).FullName;
                                break;
                            case nameof(Keyword.PostParse):
                            case nameof(Keyword.SemanticCheck):
                                paramTypeName = typeof(DynamicKeywordStatementAst).FullName;
                                break;
                        }

                        if (paramTypeName != null)
                        {
                            // Get the method signature
                            var methodSignature = methodDef.DecodeSignature(_strSigProvider, null);
                            // Parse action is defined if the signature matches -- method name, return type, parameter type/count
                            isParseActionDefined =
                                typeof(ParseError[]).FullName.Equals(methodSignature.ReturnType, StringComparison.Ordinal) &&
                                methodSignature.ParameterTypes.Length == 1 &&
                                paramTypeName.Equals(methodSignature.ParameterTypes[0], StringComparison.Ordinal);

                            // If we find an action is defined, then we have to load the assembly, so no need to search further
                            if (isParseActionDefined) { break; }
                        }
                    }

                    // If we find an action is defined, then we have to load the assembly, so no need to search further
                    if (isParseActionDefined) { break; }
                }
            }

            return isKeywordClass;
        }

        /// <summary>
        /// Read in a DynamicKeyword object from a dll specification, using the type definition and KeywordAttribute metadata on the class
        /// </summary>
        private DynamicKeyword ReadDslKeyword(TypeDefinition keywordType, CustomAttribute keywordAttribute, HashSet<string> nestedKeywordTypeNames)
        {
            string keywordName = _reader.GetString(keywordType.Name);
            string typeName = _reader.GetTypeFullName(keywordType);
            
            // DSL-TODO: This could also include `|| DynamicKeyword.Contains(keywordName)`, but this shows errors when reloading a module
            //       For now, a more local keyword will shadow a global one anyway (as you might expect)
            if (!_keywordNames_ModuleScope.Add(keywordName))
            {
                _parseErrors_ModuleScope.Add(new ParseErrorContainer(nameof(ParserStrings.DynamicKeywordMetadataKeywordAlreadyDefinedInScope),
                    ParserStrings.DynamicKeywordMetadataKeywordAlreadyDefinedInScope, keywordName));
            }
            else
            {
                _keywordToNestedKeywordsMap_AssemblyScope.Add(keywordName, nestedKeywordTypeNames);
            }

            // Read the keyword attribute
            var attributeData = ReadKeywordAttributeData(keywordAttribute);
            if (nestedKeywordTypeNames != null && attributeData.BodyMode != DynamicKeywordBodyMode.ScriptBlock)
            {
                // Only scriptblock-bodied keyword can have nested keywords
                // DSL-TODO: report error.
            }

            // Read in all parameters and properties
            var keywordParameters = new List<DynamicKeywordParameter>();
            var keywordProperties = new List<DynamicKeywordProperty>();
            foreach (var propertyHandle in keywordType.GetProperties())
            {
                var propertyDef = _reader.GetPropertyDefinition(propertyHandle);
                if (!IsPropertyQualified(propertyDef)) { continue; }

                // DSL-TODO: To make it simple for now, we stop at the first member attribute instead of validating the following cases and error out
                //        - both parameter attribute and property attribute are specified
                //        - multiple parameter attributes or property attributes are specified
                //       This needs to be improved later.
                foreach (var attributeHandle in propertyDef.GetCustomAttributes())
                {
                    var attribute = _reader.GetCustomAttribute(attributeHandle);
                    if (_reader.IsAttributeOfType(attribute, typeof(KeywordParameterAttribute)))
                    {
                        // Hashtable-bodied keywords cannot have parameters
                        if (attributeData.BodyMode == DynamicKeywordBodyMode.Hashtable)
                        {
                            _parseErrors_ModuleScope.Add(new ParseErrorContainer(nameof(ParserStrings.DynamicKeywordMetadataNonCommandKeywordHasParameters),
                                ParserStrings.DynamicKeywordMetadataNonCommandKeywordHasParameters, keywordName));
                        }

                        if (_parseErrors_ModuleScope.Count == 0)
                        {
                            keywordParameters.Add(ReadKeywordParameter(propertyDef, attribute));
                        }

                        break;
                    }
                    else if (_reader.IsAttributeOfType(attribute, typeof(KeywordPropertyAttribute)))
                    {
                        // Only Hashtable-bodied keywords can have properties
                        if (attributeData.BodyMode != DynamicKeywordBodyMode.Hashtable)
                        {
                            _parseErrors_ModuleScope.Add(new ParseErrorContainer(nameof(ParserStrings.DynamicKeywordMetadataNonHashtableKeywordHasProperties),
                                ParserStrings.DynamicKeywordMetadataNonHashtableKeywordHasProperties, keywordName, attributeData.BodyMode));
                        }

                        if (_parseErrors_ModuleScope.Count == 0)
                        {
                            keywordProperties.Add(ReadKeywordProperty(propertyDef, attribute));
                        }

                        break;
                    }
                }
            }

            if (_parseErrors_ModuleScope.Count == 0) { return null; }

            // Finally, construct the keyword
            var keyword = new DynamicKeyword()
            {
                Keyword = keywordName,
                NameMode = attributeData.NameMode,
                BodyMode = attributeData.BodyMode,
                UseMode = attributeData.UseMode,
                ResourceName = attributeData.ResourceName,
                DirectCall = attributeData.DirectCall,
                MetaStatement = attributeData.MetaStatement,
                ImplementingModule = _moduleInfo.Name,
                ImplementingModuleVersion = _moduleInfo.Version,
                ImplementingModuleInfo = _moduleInfo,
                IsReservedKeyword = false,
                HasReservedProperties = false
            };

            foreach (var parameter in keywordParameters)
            {
                keyword.Parameters.Add(parameter.Name, parameter);
            }

            foreach (var property in keywordProperties)
            {
                keyword.Properties.Add(property.Name, property);
            }

            return keyword;
        }

        /// <summary>
        /// Check if the property is a qualified keyword member.
        /// </summary>
        private bool IsPropertyQualified(PropertyDefinition propertyDef)
        {
            var accessors = propertyDef.GetAccessors();
            if (accessors.Getter.IsNil || accessors.Setter.IsNil)
            {
                // Both getter and setter should be available
                return false;
            }

            var getterMethodDef = _reader.GetMethodDefinition(accessors.Getter);
            var setterMethodDef = _reader.GetMethodDefinition(accessors.Setter);
            
            if ((getterMethodDef.Attributes & MethodAttributes.MemberAccessMask) != MethodAttributes.Public ||
                (setterMethodDef.Attributes & MethodAttributes.MemberAccessMask) != MethodAttributes.Public)
            {
                // Both getter and setter should be public
                return false;
            }

            if ((getterMethodDef.Attributes & MethodAttributes.Static) == MethodAttributes.Static ||
                (getterMethodDef.Attributes & MethodAttributes.Virtual) == MethodAttributes.Virtual)
            {
                // Ignore Static and Virtual property
                return false;
            }

            return true;
        }

        /// <summary>
        /// Read the parameters in a KeywordAttribute declaration, to pass on the the DynamicKeyword object that is created
        /// </summary>
        /// <param name="keywordAttribute">the KeywordAttribute metadata</param>
        private KeywordAttribute ReadKeywordAttributeData(CustomAttribute keywordAttribute)
        {
            CustomAttributeValue<string> attrValue = keywordAttribute.DecodeValue(_attrNameProvider);
            KeywordAttribute keywordAttr = new KeywordAttribute();

            foreach (var attrNamedArg in attrValue.NamedArguments)
            {
                switch (attrNamedArg.Name)
                {
                    case nameof(KeywordAttribute.NameMode):
                        keywordAttr.NameMode = (DynamicKeywordNameMode)attrNamedArg.Value;
                        break;

                    case nameof(KeywordAttribute.BodyMode):
                        keywordAttr.BodyMode = (DynamicKeywordBodyMode)attrNamedArg.Value;
                        break;

                    case nameof(KeywordAttribute.UseMode):
                        keywordAttr.UseMode = (DynamicKeywordUseMode)attrNamedArg.Value;
                        break;

                    case nameof(KeywordAttribute.ResourceName):
                        keywordAttr.ResourceName = (string)attrNamedArg.Value;
                        break;

                    case nameof(KeywordAttribute.DirectCall):
                        keywordAttr.DirectCall = (bool)attrNamedArg.Value;
                        break;

                    case nameof(KeywordAttribute.MetaStatement):
                        keywordAttr.MetaStatement = (bool)attrNamedArg.Value;
                        break;
                }
            }

            return keywordAttr;
        }

        /// <summary>
        /// Read in a DynamicKeywordParameter object from dll metadata, using the C# property definition and its parameter attribute
        /// </summary>
        /// <param name="propertyDef">the property metadata being read in as a dynamic keyword parameter</param>
        /// <param name="keywordParameterAttribute">the attribute on the property defining its parameters and declaring it as a parameter</param>
        /// <returns></returns>
        private DynamicKeywordParameter ReadKeywordParameter(PropertyDefinition propertyDef, CustomAttribute keywordParameterAttribute)
        {
            const string switchParaFullName = "System.Management.Automation.SwitchParameter";
            
            string parameterName = _reader.GetString(propertyDef.Name);
            string parameterType = propertyDef.DecodeSignature(_strSigProvider, null).ReturnType;

            bool mandatory = false;
            CustomAttributeValue<string> parameterAttribute = keywordParameterAttribute.DecodeValue(_attrNameProvider);
            foreach (var namedArg in parameterAttribute.NamedArguments)
            {
                if (nameof(KeywordParameterAttribute.Mandatory).Equals(namedArg.Name, StringComparison.Ordinal))
                {
                    mandatory = (bool)namedArg.Value;
                    break;
                }
            }

            var keywordParameter = new DynamicKeywordParameter()
            {
                Name = parameterName,
                TypeConstraint = parameterType,
                Mandatory = mandatory,
                Switch = switchParaFullName.Equals(parameterType, StringComparison.Ordinal)
            };

            // If the parameter has an enum type, set the values it can take
            TrySetValuesForKeywordMember(keywordParameter);
            return keywordParameter;
        }

        /// <summary>
        /// Read in a DynamicKeywordProperty object from dll metadata, using the C# property definition and its property attribute
        /// </summary>
        /// <param name="propertyDef">the property being read in as a dynamic keyword property</param>
        /// <param name="keywordPropertyAttribute">the attribute on the property defining it as a dynamic keyword property</param>
        /// <returns>a fully formed dynamic keyword property</returns>
        private DynamicKeywordProperty ReadKeywordProperty(PropertyDefinition propertyDef, CustomAttribute keywordPropertyAttribute)
        {
            // DSL-TODO: need to detect if property name is a reserved property name?
            string propertyName = _reader.GetString(propertyDef.Name);
            string propertyType = propertyDef.DecodeSignature(_strSigProvider, null).ReturnType;

            // Read in properties set in the attribute
            bool mandatory = false;
            CustomAttributeValue<string> propertyAttribute = keywordPropertyAttribute.DecodeValue(_attrNameProvider);
            foreach (var namedArg in propertyAttribute.NamedArguments)
            {
                if (nameof(KeywordPropertyAttribute.Mandatory).Equals(namedArg.Name, StringComparison.Ordinal))
                {
                    mandatory = (bool)namedArg.Value;
                    break;
                }
            }

            var keywordProperty = new DynamicKeywordProperty()
            {
                Name = propertyName,
                TypeConstraint = propertyType,
                Mandatory = mandatory
            };

            // If the property has an enum type, set its possible values
            TrySetValuesForKeywordMember(keywordProperty);
            return keywordProperty;
        }

        /// <summary>
        /// Check if a given type definition has a zero-argument constructor -- without which we cannot build it
        /// </summary>
        /// <param name="typeDef">the type definition to look for the constructor on</param>
        /// <returns>true if a zero-arg constructor is found, false otherwise</returns>
        private bool HasPublicDefaultConstructor(TypeDefinition typeDef)
        {
            const string ctorName = ".ctor";

            foreach (MethodDefinitionHandle methodHandle in typeDef.GetMethods())
            {
                MethodDefinition methodDef = _reader.GetMethodDefinition(methodHandle);

                if ((methodDef.Attributes & MethodAttributes.MemberAccessMask) != MethodAttributes.Public ||
                    !ctorName.Equals(_reader.GetString(methodDef.Name), StringComparison.Ordinal))  
                {
                    // Method is not public or is not a constructor
                    continue;
                }

                MethodSignature<string> methodSig = methodDef.DecodeSignature(_strSigProvider, null);
                if (methodSig.RequiredParameterCount == 0 && methodSig.ParameterTypes.Length == 0)
                {
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Use the TypeConstraint property on a DynamicKeywordProperty to try and
        /// set the possible values for its type
        /// </summary>
        /// <param name="keywordProperty">the property to set the enumerated types on</param>
        /// <returns>true if the values were successfully set, false otherwise</returns>
        private bool TrySetValuesForKeywordMember(DynamicKeywordProperty keywordProperty)
        {
            if (String.IsNullOrEmpty(keywordProperty.TypeConstraint))
            {
                return false;
            }

            // See if the type is an enum defined in the same DSL schema assembly.
            if (_enumTypeNameToMembers_AssemblyScope.TryGetValue(keywordProperty.TypeConstraint, out List<string> values))
            {
                keywordProperty.Values.AddRange(values);
                return true;
            }

            // We try to resolve the type and see if it's an enum.
            if (LanguagePrimitives.TryConvertTo(keywordProperty.TypeConstraint, out Type type) && type.IsEnum)
            {
                keywordProperty.Values.AddRange(Enum.GetNames(type));
                return true;
            }

            return false;
        }
    }

    internal class KeywordVertex
    {
        internal readonly string Name;
        internal int Index;
        internal int LowLink;
        internal bool OnStack;

        internal KeywordVertex(string keywordName)
        {
            Name = keywordName;
            Index = -1;
            LowLink = -1;
            OnStack = false;
        }

        /// <summary>
        /// Tarjan's strongly connected components algorithm.
        /// See https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
        /// </summary>
        internal static List<List<string>> GetNestedKeywordLoops(Dictionary<KeywordVertex, List<KeywordVertex>> graph)
        {
            int index = 0;
            Stack<KeywordVertex> stack = new Stack<KeywordVertex>(graph.Count);
            List<List<string>> loops = new List<List<string>>();
            
            foreach (var vertex in graph.Keys)
            {
                if (vertex.Index == -1)
                {
                    FindStronglyConnectComponents(vertex);
                }
            }

            void FindStronglyConnectComponents(KeywordVertex vertex)
            {
                // Set the depth index for vertex to the smallest unused index
                vertex.Index = index;
                vertex.LowLink = index;
                index ++;

                var nestedNodes = graph[vertex];
                // Current vertex doesn't point to any other vertices, so just return
                if (nestedNodes == null) { return; }

                stack.Push(vertex);
                vertex.OnStack = true;

                // Consider successors of vertex
                foreach (var next in nestedNodes)
                {
                    if (next.Index == -1)
                    {
                        // Successor 'next' has not yet been visited; recurse on it
                        FindStronglyConnectComponents(next);
                        vertex.LowLink = vertex.LowLink < next.LowLink ? vertex.LowLink : next.LowLink;
                    }
                    else if (next.OnStack)
                    {
                        // Successor 'next' is in stack and hence in the current SCC
                        // Note: The next line may look odd - but is correct.
                        // It says next.index not next.lowlink; that is deliberate and from the original paper
                        vertex.LowLink = vertex.LowLink < next.Index ? vertex.LowLink : next.Index;
                    }
                }

                // If vertex is a root node, pop the stack and generate an SCC
                if (vertex.LowLink == vertex.Index)
                {
                    List<string> loop = new List<string>();
                    KeywordVertex current = null;
                    do {
                        current = stack.Pop();
                        current.OnStack = false;
                        loop.Add(current.Name);
                    } while (current != vertex);

                    loops.Add(loop);
                }
            }

            return loops;
        }
    }
}