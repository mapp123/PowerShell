/********************************************************************++
Copyright (c) Microsoft Corporation.  All rights reserved.
--********************************************************************/

using System.Collections.Generic;
using System.Linq;
using Microsoft.PowerShell.DesiredStateConfiguration.Internal;

namespace System.Management.Automation.Language
{
    /// <summary>
    /// An abstract class to extend in order to write a keyword specification. Give this class the KeywordAttribute
    /// in order to use it as a PowerShell DynamicKeyword specification. Any properties with
    /// the KeywordParameterAttribute will be parameters, while properties with the KeywordPropertyAttribute will
    /// be keyword properties.
    /// </summary>
    public abstract class Keyword
    {
        /// <summary>
        /// Constructs a keyword with null runtime delegates. This is intended to be overridden.
        /// </summary>
        protected Keyword()
        {
        }

        /// <summary>
        /// Specifies the call to run on a DynamicKeyword data object before the AST node
        /// containing that keyword is parsed
        /// </summary>
        public virtual ParseError[] PreParse(DynamicKeyword dynamicKeyword) { return null; }

        /// <summary>
        /// Specifies the call to run on a DynamicKeyword statement AST immediately after that AST node
        /// has been parsed
        /// </summary>
        public virtual ParseError[] PostParse(DynamicKeywordStatementAst dynamicKeywordStatementAst) { return null; }

        /// <summary>
        /// Specifies the call to run on a DynamicKeyword statement AST node at semantic check time (to perform
        /// any user-specified semantic checks)
        /// </summary>
        public virtual ParseError[] SemanticCheck(DynamicKeywordStatementAst dynamicKeywordStatementAst) { return null; }
    }

    /// <summary>
    /// Defines the name modes for a dynamic keyword. A name expression may be required, optional or not permitted.
    /// </summary>
    public enum DynamicKeywordNameMode
    {
        /// <summary>
        /// This keyword does not take a name value
        /// </summary>
        NoName = 0,
        /// <summary>
        /// Name must be present and simple non-empty bare word
        /// </summary>
        SimpleNameRequired = 1,
        /// <summary>
        /// Name must be present but can also be an expression
        /// </summary>
        NameRequired = 2,
        /// <summary>
        /// Name may be optionally present, but if it is present, it must be a non-empty bare word.
        /// </summary>
        SimpleOptionalName = 3,
        /// <summary>
        /// Name may be optionally present, expression or bare word
        /// </summary>
        OptionalName = 4,
    };

    /// <summary>
    /// Defines the use semantics of a dynamic keyword for a given block
    /// </summary>
    public enum DynamicKeywordUseMode
    {
        /// <summary>
        /// The keyword must be used exactly once in a block
        /// </summary>
        Required = 0,

        /// <summary>
        /// The keyword must be used at least once in a block
        /// </summary>
        RequiredMany = 1,

        /// <summary>
        /// The keyword may be used 0 or 1 times in a block
        /// </summary>
        Optional = 2,

        /// <summary>
        /// The keyword may be used zero or more times in a block (i.e. there are no use restrictions)
        /// </summary>
        OptionalMany = 3,
    }

    /// <summary>
    /// Defines the body mode for a dynamic keyword. It can be a scriptblock, hashtable or command which means no body
    /// </summary>
    public enum DynamicKeywordBodyMode
    {
        /// <summary>
        /// The keyword act like a command
        /// </summary>
        Command = 0,
        /// <summary>
        /// The keyword has a scriptblock body
        /// </summary>
        ScriptBlock = 1,
        /// <summary>
        /// The keyword has hashtable body
        /// </summary>
        Hashtable = 2,
    }

    /// <summary>
    /// Defines the schema/behaviour for a dynamic keyword.
    /// a constrained
    /// </summary>
    public class DynamicKeyword
    {
        #region static properties/functions

        /// <summary>
        /// Defines a dictionary of dynamic keywords, stored in thread-local storage.
        /// </summary>
        private static Dictionary<string, DynamicKeyword> DynamicKeywords
        {
            get
            {
                return t_dynamicKeywords ??
                       (t_dynamicKeywords = new Dictionary<string, DynamicKeyword>(StringComparer.OrdinalIgnoreCase));
            }
        }

        [ThreadStatic]
        private static Dictionary<string, DynamicKeyword> t_dynamicKeywords;

        /// <summary>
        /// stack of DynamicKeywords Cache
        /// </summary>
        ///
        private static Stack<Dictionary<string, DynamicKeyword>> DynamicKeywordsStack
        {
            get
            {
                return t_dynamicKeywordsStack ??
                       (t_dynamicKeywordsStack = new Stack<Dictionary<string, DynamicKeyword>>());
            }
        }
        [ThreadStatic]
        private static Stack<Dictionary<string, DynamicKeyword>> t_dynamicKeywordsStack;

        /// <summary>
        /// Reset the keyword table to a new empty collection.
        /// </summary>
        public static void Reset()
        {
            t_dynamicKeywords = new Dictionary<string, DynamicKeyword>(StringComparer.OrdinalIgnoreCase);
        }

        /// <summary>
        /// Push current dynamicKeywords cache into stack.
        /// </summary>
        /// <remarks>
        /// This method is used to temporarily hide dynamic keywords from a parsing so that
        /// no existing dynamic keyword will take effect during that parsing.
        /// </remarks>
        public static void Push()
        {
            DynamicKeywordsStack.Push(t_dynamicKeywords);
            Reset();
        }

        /// <summary>
        /// Pop up previous dynamicKeywords cache
        /// </summary>
        public static void Pop()
        {
            t_dynamicKeywords = DynamicKeywordsStack.Pop();
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        public static DynamicKeyword GetKeyword(string name)
        {
            DynamicKeyword keywordToReturn;
            DynamicKeyword.DynamicKeywords.TryGetValue(name, out keywordToReturn);
            return keywordToReturn;
        }

        /// <summary>
        /// Returns a copied list of all of the existing dynamic keyword definitions.
        /// </summary>
        /// <returns></returns>
        public static List<DynamicKeyword> GetKeyword()
        {
            return new List<DynamicKeyword>(DynamicKeyword.DynamicKeywords.Values);
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        public static bool ContainsKeyword(string name)
        {
            if (string.IsNullOrEmpty(name))
            {
                PSArgumentNullException e = PSTraceSource.NewArgumentNullException("name");
                throw e;
            }

            return DynamicKeyword.DynamicKeywords.ContainsKey(name);
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="keywordToAdd"></param>
        public static void AddKeyword(DynamicKeyword keywordToAdd)
        {
            if (keywordToAdd == null)
            {
                PSArgumentNullException e = PSTraceSource.NewArgumentNullException("keywordToAdd");
                throw e;
            }

            // Allow overwriting of the existing entries
            string name = keywordToAdd.Keyword;
            if (string.IsNullOrEmpty(name))
            {
                throw PSTraceSource.NewArgumentNullException("keywordToAdd.Keyword");
            }

            DynamicKeyword.DynamicKeywords.Remove(name);
            DynamicKeyword.DynamicKeywords.Add(name, keywordToAdd);
        }

        /// <summary>
        /// Remove a single entry from the dynamic keyword collection
        /// and clean up any associated data.
        /// </summary>
        /// <param name="name"></param>
        public static void RemoveKeyword(string name)
        {
            if (string.IsNullOrEmpty(name))
            {
                PSArgumentNullException e = PSTraceSource.NewArgumentNullException("name");
                throw e;
            }
            DynamicKeyword.DynamicKeywords.Remove(name);
        }

        /// <summary>
        /// Check if it is a hidden keyword
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        internal static bool IsHiddenKeyword(string name)
        {
            if (string.IsNullOrEmpty(name))
            {
                PSArgumentNullException e = PSTraceSource.NewArgumentNullException("name");
                throw e;
            }

            return s_hiddenDynamicKeywords.Contains(name);
        }

        /// <summary>
        /// A set of dynamic keywords that are not supposed to be used in script directly.
        /// They are for internal use only.
        /// </summary>
        private static readonly HashSet<string> s_hiddenDynamicKeywords =
            new HashSet<string>(StringComparer.OrdinalIgnoreCase) { "MSFT_Credential" };

        #endregion

        /// <summary>
        /// Duplicates the DynamicKeyword
        /// </summary>
        /// <returns>A copy of the DynamicKeyword</returns>
        public DynamicKeyword Copy()
        {
            DynamicKeyword keyword = new DynamicKeyword()
            {
                ImplementingModule = this.ImplementingModule,
                ImplementingModuleVersion = this.ImplementingModuleVersion,
                Keyword = this.Keyword,
                ResourceName = this.ResourceName,
                BodyMode = this.BodyMode,
                DirectCall = this.DirectCall,
                NameMode = this.NameMode,
                MetaStatement = this.MetaStatement,
                IsReservedKeyword = this.IsReservedKeyword,
                HasReservedProperties = this.HasReservedProperties,
                PreParse = this.PreParse,
                PostParse = this.PostParse,
                SemanticCheck = this.SemanticCheck
            };
            foreach (KeyValuePair<string, DynamicKeywordProperty> entry in this.Properties)
            {
                keyword.Properties.Add(entry.Key, entry.Value);
            }
            foreach (KeyValuePair<string, DynamicKeywordParameter> entry in this.Parameters)
            {
                keyword.Parameters.Add(entry.Key, entry.Value);
            }
            return keyword;
        }

        /// <summary>
        /// The name of the module that implements the function corresponding to this keyword.
        /// </summary>
        public string ImplementingModule { get; set; }

        /// <summary>
        /// The version of the module that implements the function corresponding to this keyword.
        /// </summary>
        public Version ImplementingModuleVersion { get; set; }

        /// <summary>
        /// The info object representing all the necessary data on the module this keyword was loaded from
        /// </summary>
        public PSModuleInfo ImplementingModuleInfo { get; set; }

        /// <summary>
        /// The keyword string
        /// If an alias qualifier exist, use alias
        /// </summary>
        public string Keyword { get; set; }

        /// <summary>
        /// The keyword resource name string
        /// </summary>
        public string ResourceName { get; set; }

        /// <summary>
        /// Set the body mode of the DynamicKeyword
        /// </summary>
        public DynamicKeywordBodyMode BodyMode { get; set; }

        /// <summary>
        /// If true, then don't use the marshalled call. Just
        /// rewrite the node as a simple direct function call.
        /// If NameMode is other than NoName, then the name of the instance
        /// will be passed as the parameter -InstanceName.
        ///
        /// </summary>
        public bool DirectCall { get; set; }

        /// <summary>
        /// This allows you to specify if the keyword takes a name argument and if so, what form that takes.
        /// </summary>
        public DynamicKeywordNameMode NameMode { get; set; }

        /// <summary>
        /// Specifies how many times a keyword may be used within its parent keyword scope.
        /// </summary>
        public DynamicKeywordUseMode UseMode { get; set; }

        /// <summary>
        /// Indicate that the nothing should be added to the AST for this
        /// dynamic keyword.
        /// </summary>
        public bool MetaStatement { get; set; }

        /// <summary>
        /// Indicate that the keyword is reserved for future use by powershell
        /// </summary>
        public bool IsReservedKeyword { get; set; }

        /// <summary>
        /// Contains the list of properties that are reserved for future use
        /// </summary>
        public bool HasReservedProperties { get; set; }

        /// <summary>
        /// True if the keyword belongs in the scope of another keyword. False otherwise.
        /// </summary>
        public bool IsNested { get; set; }

        /// <summary>
        /// A list of the properties allowed for this constuctor
        /// </summary>
        public Dictionary<string, DynamicKeywordProperty> Properties
        {
            get
            {
                return _properties ??
                       (_properties = new Dictionary<string, DynamicKeywordProperty>(StringComparer.OrdinalIgnoreCase));
            }
        }
        private Dictionary<string, DynamicKeywordProperty> _properties;

        /// <summary>
        /// A list of the parameters allowed for this constuctor.
        /// </summary>
        public Dictionary<string, DynamicKeywordParameter> Parameters
        {
            get
            {
                return _parameters ??
                       (_parameters = new Dictionary<string, DynamicKeywordParameter>(StringComparer.OrdinalIgnoreCase));
            }
        }
        private Dictionary<string, DynamicKeywordParameter> _parameters;

        /// <summary>
        /// Keywords that are defined within the scope of this keyword
        /// </summary>
        public Dictionary<string, DynamicKeyword> InnerKeywords
        {
            get
            {
                return _innerKeywords ??
                    (_innerKeywords = new Dictionary<string, DynamicKeyword>(StringComparer.OrdinalIgnoreCase));
            }
        }
        private Dictionary<string, DynamicKeyword> _innerKeywords;

        /// <summary>
        /// A custom function that gets executed at parsing time before parsing dynamickeyword block
        /// The delegate has one parameter: DynamicKeyword
        /// </summary>
        public Func<DynamicKeyword, ParseError[]> PreParse { get; set; }

        /// <summary>
        /// A custom function that gets executed at parsing time after parsing dynamickeyword block
        /// </summary>
        public Func<DynamicKeywordStatementAst, ParseError[]> PostParse { get; set; }

        /// <summary>
        /// A custom function that checks semantic for the given <see cref="DynamicKeywordStatementAst"/>
        /// </summary>
        public Func<DynamicKeywordStatementAst, ParseError[]> SemanticCheck { get; set; }
    }


    internal static class DynamicKeywordExtension
    {
        internal static bool IsMetaDSCResource(this DynamicKeyword keyword)
        {
            string implementingModule = keyword.ImplementingModule;
            if (implementingModule != null)
            {
                return implementingModule.Equals(DscClassCache.DefaultModuleInfoForMetaConfigResource.Item1, StringComparison.OrdinalIgnoreCase);
            }
            return false;
        }

        internal static bool IsCompatibleWithConfigurationType(this DynamicKeyword keyword, ConfigurationType ConfigurationType)
        {
            return ((ConfigurationType == ConfigurationType.Meta && keyword.IsMetaDSCResource()) ||
                    (ConfigurationType != ConfigurationType.Meta && !keyword.IsMetaDSCResource()));
        }

        private static Dictionary<String, List<String>> s_excludeKeywords = new Dictionary<String, List<String>>(StringComparer.OrdinalIgnoreCase)
        {
            {@"Node", new List<String> {@"Node"}},
        };

        /// <summary>
        /// Get allowed keyword list for a given keyword
        /// </summary>
        /// <param name="keyword"></param>
        /// <param name="allowedKeywords"></param>
        /// <returns>NULL if no keyword allowed for a given <see cref="DynamicKeyword"/></returns>
        internal static IEnumerable<DynamicKeyword> GetAllowedKeywords(this DynamicKeyword keyword, IEnumerable<DynamicKeyword> allowedKeywords)
        {
            string keywordName = keyword.Keyword;
            if (String.Compare(keywordName, @"Node", StringComparison.OrdinalIgnoreCase) == 0)
            {
                List<string> excludeKeywords;
                if (s_excludeKeywords.TryGetValue(keywordName, out excludeKeywords))
                {
                    return allowedKeywords.Where(k => !excludeKeywords.Contains(k.Keyword));
                }
                else
                    return allowedKeywords;
            }
            return null;
        }
    }

    /// <summary>
    /// Metadata about a member property for a dynamic keyword.
    /// </summary>
    public class DynamicKeywordProperty
    {
        /// <summary>
        /// The name of the property
        /// </summary>
        public string Name { get; set; }

        /// <summary>
        /// The required type of the property
        /// </summary>
        public string TypeConstraint { get; set; }

        /// <summary>
        /// Any attributes that the property has
        /// </summary>
        public List<string> Attributes
        {
            get { return _attributes ?? (_attributes = new List<string>()); }
        }
        private List<string> _attributes;

        /// <summary>
        /// List of strings that may be used as values for this property.
        /// </summary>
        public List<string> Values
        {
            get { return _values ?? (_values = new List<string>()); }
        }
        private List<string> _values;

        /// <summary>
        /// Mapping the descriptive values to the actual values
        /// </summary>
        public Dictionary<string, string> ValueMap
        {
            get { return _valueMap ?? (_valueMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)); }
        }
        private Dictionary<string, string> _valueMap;

        /// <summary>
        /// Indicates that this property is mandatory and must be present.
        /// </summary>
        public bool Mandatory { get; set; }

        /// <summary>
        /// Indicates that this property is a key.
        /// </summary>
        public bool IsKey { get; set; }

        /// <summary>
        /// Indicates a range constraint on the property value
        /// </summary>
        public Tuple<int, int> Range { get; set; }
    }

    /// <summary>
    /// Metadata about a parameter for a dynamic keyword. Adds one
    /// new property to the base classL Switch for switch parameters
    /// (THere is no such thing as a switch property...)
    /// </summary>
    public class DynamicKeywordParameter : DynamicKeywordProperty
    {
        /// <summary>
        /// Type if this is a switch parameter and takes no argument
        /// </summary>
        public bool Switch { get; set; }
    }
}
