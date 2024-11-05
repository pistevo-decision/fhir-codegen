using System.Runtime.Serialization;
using Hl7.Fhir.Model;
using Microsoft.Health.Fhir.CodeGen.Configuration;
using Microsoft.Health.Fhir.CodeGen.Extensions;
using Microsoft.Health.Fhir.CodeGen.FhirExtensions;
using Microsoft.Health.Fhir.CodeGen.Models;
using Microsoft.Health.Fhir.CodeGen.Utils;
using Microsoft.Health.Fhir.CodeGenCommon.Packaging;
using Microsoft.Health.Fhir.CodeGenCommon.Utils;
using static Microsoft.Health.Fhir.CodeGenCommon.Extensions.FhirNameConventionExtensions;
using Hl7.Fhir.Utility;
using Microsoft.Health.Fhir.CodeGenCommon.Extensions;
using System.Text.RegularExpressions;





#if NETSTANDARD2_0
using Microsoft.Health.Fhir.CodeGenCommon.Polyfill;
#endif

namespace Microsoft.Health.Fhir.CodeGen.Language;

public class Dart : ILanguage
{
    private const string DefaultNamespace = "fhir{VersionNumber}";
    private const string DefaultMinDartVersion = "2.12";

    public Type ConfigType => typeof(DartOptions);

    public class DartOptions : ConfigGenerate
    {
        /// <summary>Gets or sets the namespace.</summary>
        [ConfigOption(
            ArgName = "--namespace",
            Description = "Base namespace for Dart files, default is 'fhir{VersionNumber}', use '' (empty string) for none."
        )]
        public string Namespace { get; set; } = DefaultNamespace;

        private static ConfigurationOption NamespaceParameter { get; } =
            new()
            {
                Name = "Namespace",
                DefaultValue = DefaultNamespace,
                CliOption = new System.CommandLine.Option<string>(
                    "--namespace",
                    "Base namespace for Dart files, default is 'fhir{VersionNumber}', use '' (empty string) for none."
                )
                {
                    Arity = System.CommandLine.ArgumentArity.ZeroOrOne,
                    IsRequired = false,
                },
            };

        /// <summary>Gets or sets the minimum dart version.</summary>
        [ConfigOption(
            ArgName = "--min-dart-version",
            Description = "Minimum Dart version, use '' (empty string) for none."
        )]
        public string MinDartVersion { get; set; } = DefaultMinDartVersion;

        private static ConfigurationOption MinDartVersionParameter { get; } =
            new()
            {
                Name = "MinDartVersion",
                DefaultValue = DefaultMinDartVersion,
                CliOption = new System.CommandLine.Option<string>(
                    "--min-Dart-version",
                    "Minimum Dart version, use '' (empty string) for none."
                )
                {
                    Arity = System.CommandLine.ArgumentArity.ZeroOrOne,
                    IsRequired = false,
                },
            };

        /// <summary>Gets or sets the namespace.</summary>
        [ConfigOption(
            ArgName = "--inline-enums",
            Description = "If code elements with required bindings should have inlined enums."
        )]
        public bool InlineEnums { get; set; } = true;

        private static ConfigurationOption InlineEnumsParameter { get; } =
            new()
            {
                Name = "InlineEnums",
                DefaultValue = true,
                CliOption = new System.CommandLine.Option<string>(
                    "--inline-enums",
                    "If code elements with required bindings should have inlined enums."
                )
                {
                    Arity = System.CommandLine.ArgumentArity.ZeroOrOne,
                    IsRequired = false,
                },
            };

        /// <summary>(Immutable) Options for controlling the operation.</summary>
        private static readonly ConfigurationOption[] _options =
        [
            NamespaceParameter,
            MinDartVersionParameter,
            InlineEnumsParameter,
        ];

        /// <summary>
        /// Gets the configuration options for the current instance and its base class.
        /// </summary>
        /// <returns>An array of configuration options.</returns>
        public override ConfigurationOption[] GetOptions()
        {
            return [.. base.GetOptions(), .. _options];
        }

        public override void Parse(System.CommandLine.Parsing.ParseResult parseResult)
        {
            // parse base properties
            base.Parse(parseResult);

            // iterate over options for ones we are interested in
            foreach (ConfigurationOption opt in _options)
            {
                switch (opt.Name)
                {
                    case "Namespace":
                        Namespace = GetOpt(parseResult, opt.CliOption, Namespace);
                        break;
                    case "MinDartVersion":
                        MinDartVersion = GetOpt(parseResult, opt.CliOption, MinDartVersion);
                        break;
                    case "InlineEnums":
                        InlineEnums = GetOpt(parseResult, opt.CliOption, InlineEnums);
                        break;
                }
            }
        }

        /// <summary>Gets or sets the write stream to use.</summary>
        public Stream? WriteStream { get; set; } = null;
    }

    /// <summary>The systems named by display.</summary>
    private static HashSet<string> _systemsNamedByDisplay =
    [
        /// <summary>Units of Measure have incomprehensible codes after naming substitutions.</summary>
        "http://unitsofmeasure.org",
    ];

    private static HashSet<string> _systemsNamedByCode =
    [
        /// <summary>Operation Outcomes include c-style string formats in display.</summary>
        "http://terminology.hl7.org/CodeSystem/operation-outcome",
        /// <summary>Descriptions have quoted values.</summary>
        "http://terminology.hl7.org/CodeSystem/smart-capabilities",
        /// <summary>Descriptions have quoted values.</summary>
        "http://hl7.org/fhir/v2/0301",
        /// <summary>Display values are too long to be useful.</summary>
        "http://terminology.hl7.org/CodeSystem/v2-0178",
        /// <summary>Display values are too long to be useful.</summary>
        "http://terminology.hl7.org/CodeSystem/v2-0277",
        /// <summary>Display values are too long to be useful.</summary>
        "http://terminology.hl7.org/CodeSystem/v3-VaccineManufacturer",
        /// <summary>Display values are too long to be useful.</summary>
        "http://hl7.org/fhir/v2/0278",
        /// <summary>Display includes operation symbols: $.</summary>
        "http://terminology.hl7.org/CodeSystem/testscript-operation-codes",
        /// <summary>Display are often just symbols.</summary>
        "http://hl7.org/fhir/v2/0290",
        /// <summary>Display includes too many Unicode characters (invalid names).</summary>
        "http://hl7.org/fhir/v2/0255",
        /// <summary>Display includes too many Unicode characters (invalid names).</summary>
        "http://hl7.org/fhir/v2/0256",
    ];

    /// <summary>FHIR information we are exporting.</summary>
    private DefinitionCollection _dc = null!;

    /// <summary>Options for controlling the export.</summary>
    private DartOptions _options = null!;

    /// <summary>True to enums.</summary>
    private bool _exportEnums;

    /// <summary>The exported codes.</summary>
    private HashSet<string> _exportedCodes = [];

    /// <summary>The exported resources.</summary>
    private List<string> _exportedResources = [];

    /// <summary>The exported file names.</summary>
    private List<string> _exportedSrcFileNames = [];

    /// <summary>The currently in-use text writer.</summary>
    private ExportStreamWriter _writer = null!;

    /// <summary>The single file extension.</summary>
    private const string SingleFileExportExtension = ".dart";

    /// <summary>The minimum type script version.</summary>
    private string _minimumDartVersion = "2.12";

    /// <summary>Dictionary mapping FHIR primitive types to language equivalents.</summary>
    private static readonly Dictionary<string, string> _primitiveTypeMap =
        new()
        {
            { "base", "Object" },
            { "base64Binary", "String" },
            { "boolean", "bool" },
            { "canonical", "String" },
            { "code", "String" },
            { "date", "String" },
            { "dateTime", "String" },
            { "decimal", "num" },
            { "id", "String" },
            { "instant", "String" },
            { "integer", "int" },
            { "integer64", "String" },
            { "markdown", "String" },
            { "oid", "String" },
            { "positiveInt", "int" },
            { "string", "String" },
            { "time", "String" },
            { "unsignedInt", "int" },
            { "uri", "String" },
            { "url", "String" },
            { "uuid", "String" },
            { "xhtml", "String" },
        };

    /// <summary>Gets the reserved words.</summary>
    /// <value>The reserved words.</value>
    private static readonly HashSet<string> _reservedWords =
    [
        "abstract",
        "as",
        "assert",
        "async",
        "await",
        "break",
        "case",
        "catch",
        "class",
        "const",
        "continue",
        "covariant",
        "default",
        "deferred",
        "do",
        "dynamic",
        "else",
        "enum",
        "export",
        "extends",
        "extension",
        "external",
        "factory",
        "false",
        "final",
        "finally",
        "for",
        "Function",
        "get",
        "hide",
        "if",
        "implements",
        "import",
        "in",
        "interface",
        "is",
        "late",
        "library",
        "mixin",
        "new",
        "null",
        "on",
        "operator",
        "part",
        "rethrow",
        "return",
        "set",
        "show",
        "static",
        "super",
        "switch",
        "sync",
        "this",
        "throw",
        "true",
        "try",
        "typedef",
        "var",
        "void",
        "while",
        "with",
        "yield",
        "List",
    ];

    private static readonly HashSet<string> _unexportableSystems =
    [
        "http://www.rfc-editor.org/bcp/bcp13.txt",
        "http://hl7.org/fhir/ValueSet/mimetype",
        "http://hl7.org/fhir/ValueSet/mimetypes",
        "http://hl7.org/fhir/ValueSet/ucum-units",
    ];

    /// <summary>The generics and type hints.</summary>
    private static readonly Dictionary<string, GenericTypeHintInfo> _genericsAndTypeHints =
        new()
        {
            {
                "Bundle.entry.resource",
                new GenericTypeHintInfo()
                {
                    Alias = "FhirResource",
                    GenericHint = string.Empty,
                    IncludeBase = false,
                }
            },
        };

    /// <summary>Gets the name of the language.</summary>
    /// <value>The name of the language.</value>
    public string Name => "Dart";

    /// <summary>Gets the FHIR primitive type map.</summary>
    /// <value>The FHIR primitive type map.</value>
    public Dictionary<string, string> FhirPrimitiveTypeMap => _primitiveTypeMap;

    /// <summary>Gets a value indicating whether this language is idempotent.</summary>
    public bool IsIdempotent => true;

    /// <summary>Gets the reserved words.</summary>
    /// <value>The reserved words.</value>
    public HashSet<string> ReservedWords => _reservedWords;

    /// <summary>Export the passed FHIR version into the specified directory.</summary>
    /// <param name="info">           The information.</param>
    /// <param name="serverInfo">     Information describing the server.</param>
    /// <param name="options">        Options for controlling the operation.</param>
    /// <param name="exportDirectory">Directory to write files.</param>
    public void Export(object untypedConfig, DefinitionCollection definitions)
    {
        if (untypedConfig is not DartOptions config)
        {
            throw new ArgumentException("Invalid configuration type");
        }

        // set internal vars so we don't pass them to every function
        // this is ugly, but the class patterns get bad quickly because we need the type map to copy the FHIR info
        _dc = definitions;
        _options = config;

        _minimumDartVersion = config.MinDartVersion;

        _exportedCodes = [];
        _exportedResources = [];

        if (config.ExportStructures.Contains(CodeGenCommon.Models.FhirArtifactClassEnum.ValueSet))
        {
            _exportEnums = !_options.InlineEnums;
        }
        else
        {
            _exportEnums = false;
        }


        WriteStructures(_dc.ComplexTypesByName.Values, false);
        WriteStructures(_dc.ResourcesByName.Values, true);

        if (_exportEnums)
        {
            WriteValueSets(_dc.ValueSetsByVersionedUrl.Values);
        }

        WriteExpandedResourceClassBinding();

        WriteLibraryClass();

        DisposeWriter();

    }

    private void WriteLibraryClass()
    {
        ChangeWriterFile("fhir_dart", true);

        _writer.WriteLineIndented("library fhir_dart;");
        _writer.WriteLineIndented("");

        // loop over exported files and add part statements
        foreach (string file in _exportedSrcFileNames)
        {
            _writer.WriteLineIndented($"part 'src/{file}.dart';");
        }
    }


    /// <summary>Writes the expanded resource class binding.</summary>
    private void WriteExpandedResourceClassBinding()
    {
        if (_exportedResources.Count == 0)
        {
            return;
        }

        _exportedResources.Sort();

        ChangeWriterFile("fhir_resource");

        _writer.WriteLineIndented("part of '../fhir_dart.dart';");
        _writer.WriteLineIndented("");

        // foreach (string resource in _exportedResources)
        // {
        //     // import the resources
        //     _writer.WriteLine($"import '{ToLowerUnderscore(resource)}.dart';");
        // }

        _writer.WriteLineIndented("abstract class FhirResource {");
        // factory

        _writer.WriteLineIndented("factory FhirResource.fromJson(Map<String, dynamic> json) {");
        _writer.IncreaseIndent();
        // switch case
        _writer.WriteLineIndented("switch (json['resourceType']) {");
        _writer.IncreaseIndent();
        // loop over exported resources
        foreach (string resource in _exportedResources)
        {
            _writer.WriteLineIndented($"case {resource}.resourceType:");
            _writer.IncreaseIndent();
            _writer.WriteLineIndented($"return {resource}.fromJson(json);");
            _writer.DecreaseIndent();
        }
        // default case
        _writer.WriteLineIndented("default:");
        _writer.IncreaseIndent();
        _writer.WriteLineIndented("throw Exception('Invalid resource type: ${json['resourceType']}');");
        _writer.DecreaseIndent();
        _writer.DecreaseIndent();
        _writer.WriteLineIndented("}");
        _writer.DecreaseIndent();
        _writer.WriteLineIndented("}");


        _writer.WriteLineIndented("Map<String, dynamic> toJson();");


        _writer.WriteLineIndented("}");
    }

    /// <summary>Writes a value sets.</summary>
    /// <param name="valueSets">List of valueSetCollections.</param>
    private void WriteValueSets(IEnumerable<ValueSet> valueSets)
    {
        Dictionary<string, WrittenCodeInfo> writtenCodesAndNames = [];
        HashSet<string> writtenNames = [];

        foreach (ValueSet vs in valueSets.OrderBy(v => v.Url))
        {
            // only write expansions

            ValueSet? expanded = _dc.ExpandVs(vs.Url).Result;

            if (expanded == null)
            {
                continue;
            }

            WriteValueSet(expanded, ref writtenCodesAndNames, ref writtenNames);
        }
    }

    /// <summary>Writes a value set.</summary>
    /// <param name="vs">                  The value set.</param>
    /// <param name="writtenCodesAndNames">[in,out] The written codes, to prevent duplication
    ///  without writing all code systems.</param>
    /// <param name="writtenNames">        [in,out] List of names of ValueSets that have been written.</param>
    private void WriteValueSet(
        ValueSet vs,
        ref Dictionary<string, WrittenCodeInfo> writtenCodesAndNames,
        ref HashSet<string> writtenNames
    )
    {
        string vsName = FhirSanitizationUtils
            .SanitizeForProperty(vs.Id ?? vs.Name, _reservedWords)
            .ToPascalCase();

        IEnumerable<FhirConcept> concepts = vs.cgGetFlatConcepts(_dc).OrderBy(c => c.Key);

        foreach (FhirConcept concept in concepts)
        {
            if (writtenCodesAndNames.ContainsKey(concept.Key))
            {
                continue;
            }

            string input = concept.Display;
            if (_systemsNamedByDisplay.Contains(concept.System))
            {
                input = concept.Display;
            }
            else if (_systemsNamedByCode.Contains(concept.System))
            {
                input = concept.Code;
            }
            else if (string.IsNullOrEmpty(input))
            {
                input = concept.Code;
            }

            string codeName = FhirSanitizationUtils
                .SanitizeForProperty(input, _reservedWords)
                .ToPascalCase(true);
            string codeValue = FhirSanitizationUtils.SanitizeForValue(concept.Code);

            string systemLocalName = concept.cgSystemLocalName();

            string constName;
            if (!string.IsNullOrEmpty(systemLocalName))
            {
                constName = $"{systemLocalName}_{codeName}";
            }
            else
            {
                constName = $"{vsName}_{codeName}";
            }

            if (writtenNames.Contains(constName))
            {
                // start at 2 so that the unadorned version makes sense as v1
                for (int i = 2; i < 1000; i++)
                {
                    if (writtenNames.Contains($"{constName}_{i}"))
                    {
                        continue;
                    }

                    constName = $"{constName}_{i}";
                    break;
                }
            }

            writtenCodesAndNames.Add(
                concept.Key,
                new WrittenCodeInfo() { Name = codeName, ConstName = constName }
            );
            writtenNames.Add(constName);

            _writer.WriteLineIndented($"const {constName}: Coding = {{");
            _writer.IncreaseIndent();

            _writer.WriteLineIndented($"code: \"{codeValue}\",");

            if (!string.IsNullOrEmpty(concept.Display))
            {
                _writer.WriteLineIndented(
                    $"display: \"{FhirSanitizationUtils.SanitizeForQuoted(concept.Display)}\","
                );
            }

            _writer.WriteLineIndented($"system: \"{concept.System}\"");

            _writer.DecreaseIndent();

            _writer.WriteLineIndented("};");
        }

        if (!string.IsNullOrEmpty(vs.Description))
        {
            WriteIndentedComment(vs.Description);
        }
        else
        {
            WriteIndentedComment($"Value Set: {_dc.VersionedUrlForVs(vs.Url)}");
        }

        _writer.WriteLineIndented($"const {vsName} = {{");
        _writer.IncreaseIndent();

        bool prefixWithSystem = vs.cgReferencedCodeSystems().Count() > 1;
        HashSet<string> usedValues = [];

        // TODO: shouldn't loop over this twice, but writer functions don't allow writing in two places at once yet
        foreach (FhirConcept concept in concepts)
        {
            string codeKey = concept.Key;
            string systemLocalName = concept.cgSystemLocalName();
            string definition = _dc.ConceptDefinition(concept.System, concept.Code);

            if (!string.IsNullOrEmpty(definition))
            {
                WriteIndentedComment(definition);
            }

            string name;

            if (prefixWithSystem)
            {
                name = $"{writtenCodesAndNames[codeKey].Name}_{systemLocalName}";
            }
            else
            {
                name = writtenCodesAndNames[codeKey].Name;
            }

            if (usedValues.Contains(name))
            {
                // start at 2 so that the unadorned version makes sense as v1
                for (int i = 2; i < 1000; i++)
                {
                    if (usedValues.Contains($"{name}_{i}"))
                    {
                        continue;
                    }

                    name = $"{name}_{i}";
                    break;
                }
            }

            usedValues.Add(name);

            _writer.WriteLineIndented($"{name}: {writtenCodesAndNames[codeKey].ConstName},");
        }

        _writer.DecreaseIndent();

        _writer.WriteLineIndented("};");
    }

    /// <summary>Writes each of the StructureDefinitions in the enumeration.</summary>
    /// <param name="structures"> The Structure Definitions.</param>
    /// <param name="isResource">(Optional) True if is resource, false if not.</param>
    private void WriteStructures(
        IEnumerable<StructureDefinition> structures,
        bool isResource = false
    )
    {
        foreach (StructureDefinition sd in structures.OrderBy(c => c.Name))
        {
            // get each component
            foreach (
                ComponentDefinition component in sd.cgComponents(_dc, null, true, false)
                    .OrderBy(c => c.Element?.Path ?? string.Empty, FhirDotNestComparer.Instance)
            )
            {
                WriteDartClassImports(component.cgNameRooted(NamingConvention.PascalCase), component);
                WriteDartClass(component, isResource && !component.Element.Path.Contains('.'));
            }
        }
    }

    private void WriteDartClassImports(string className, ComponentDefinition cd)
    {

        // if class name is reserved, add a Fhir prefix
        if (ReservedWords.Contains(className))
        {
            className = $"Fhir{className}";
        }
        // Change the writer to the class file
        ChangeWriterFile(ToLowerUnderscore(className));
        _writer.WriteLineIndented("part of '../fhir_dart.dart';");

        _writer.WriteLineIndented("");

        // Create a HashSet to hold unique property types
        HashSet<string> propertyTypes = new HashSet<string>();

        // Add property types from the current class
        AddPropertyTypes(cd, propertyTypes);

        // Check for inherited class types
        string baseTypeName = GetBaseTypeName(cd);
        if (!string.IsNullOrEmpty(baseTypeName) && !FhirPrimitiveTypeMap.ContainsValue(baseTypeName))
        {
            propertyTypes.Add(baseTypeName);
        }

        if (cd.Element.Path != "Element")
        {
            // Add FhirResource if applicable
            if (ShouldWriteResourceType(cd.Structure.Name))
            {
                propertyTypes.Add("FhirResource");
            }
        }


        // Write import statements for each unique property type
        // foreach (var propertyType in propertyTypes)
        // {
        //     // Import the property type unless it's a primitive type or matches the class name
        //     if (!FhirPrimitiveTypeMap.ContainsValue(propertyType) && propertyType != className)
        //     {
        //         _writer.WriteLineIndented($"import '{ToLowerUnderscore(propertyType)}.dart';");
        //     }
        // }
    }


    private void DisposeWriter()
    {
        _writer?.Dispose();
    }

    // Helper method to gather property types from the ComponentDefinition

    private void AddPropertyTypes(ComponentDefinition cd, HashSet<string> propertyTypes)
    {
        // Add types from elements in the ComponentDefinition
        foreach (ElementDefinition element in cd.Structure.cgElements(cd.Element.Path, true, false).OrderBy(e => e.Path))
        {
            // Get values for export
            Dictionary<string, string> values = NamesAndTypesForExport(element);

            // Check for each value if it's a generic type or should be imported
            foreach ((string exportName, string elementTypes) in values)
            {
                // Add the elementTypes to the propertyTypes set if it's not a primitive type
                if (!FhirPrimitiveTypeMap.ContainsValue(elementTypes))
                {
                    // Check if it's a generic type hint
                    if (_genericsAndTypeHints.TryGetValue(element.Path, out GenericTypeHintInfo hint))
                    {
                        // Only add the alias for generic types
                        propertyTypes.Add(hint.Alias);
                    }
                    else
                    {
                        // Add the actual elementTypes unless it's the same as the class name
                        string actualExportName = exportName;
                        if (ReservedWords.Contains(exportName))
                        {
                            actualExportName = $"fhir{exportName.Capitalize()}";
                        }

                        // Avoid importing the class if the property name is the same as the class name
                        if (actualExportName != cd.cgNameRooted(NamingConvention.PascalCase))
                        {
                            propertyTypes.Add(elementTypes);
                        }
                    }
                }
            }
        }
    }

    // Helper method to get the base type name for the current component definition
    private string GetBaseTypeName(ComponentDefinition cd)
    {
        // Assuming typeName is derived from the base structure type logic in WriteDartClass
        if (cd.IsRootOfStructure)
        {
            return cd.Structure.cgBaseTypeName(_dc, _primitiveTypeMap);
        }
        else
        {
            return cd.Element.cgBaseTypeName(_dc, false, _primitiveTypeMap);
        }
    }


    private static string BuildCommentString(StructureDefinition sd)
    {
        string[] values =
            (
                !string.IsNullOrEmpty(sd.Description)
                && !string.IsNullOrEmpty(sd.Title)
                && sd.Description.StartsWith(sd.Title)
            )
                ? new[] { sd.Description, sd.Purpose }
                : new[] { sd.Title, sd.Description, sd.Purpose };

        return string.Join("\n", values.Where(s => !string.IsNullOrEmpty(s)).Distinct()).Trim();
    }

    private static string BuildCommentString(ElementDefinition ed)
    {
        bool skipShort =
            !string.IsNullOrEmpty(ed.Definition)
            && !string.IsNullOrEmpty(ed.Short)
            && ed.Definition.StartsWith(ed.Short);

        // check for a code element and the short containing codes
        skipShort = skipShort || (ed.cgHasCodes() && ed.Short.Split('|').Length > 2);

        string[] values = skipShort
            ? new[] { ed.Definition, ed.Comment }
            : new[] { ed.Short, ed.Definition, ed.Comment };
        return string.Join("\n", values.Where(s => !string.IsNullOrEmpty(s)).Distinct()).Trim();
    }



    // ChangeWriterFile
    // dispose writer if not null and open file with the passed name 
    private void ChangeWriterFile(string file, bool root = false)
    {
        if (_writer != null)
        {
            _writer.Dispose();
        }



        string directory = _options.OutputDirectory;
        if (!root)
        {
            directory = Path.Combine(directory, "src");
            _exportedSrcFileNames.Add(file);
        }


        if (!Directory.Exists(directory))
        {
            Directory.CreateDirectory(directory);
        }


        // create a filename for writing 
        string filename = Path.Combine(directory, $"{file}.dart");

        _writer = new ExportStreamWriter(
                    new FileStream(filename, FileMode.Create),
                    System.Text.Encoding.UTF8,
                    1024,
                    true
                    );

    }


    /// <summary>Writes a StructureDefinition.</summary>
    /// <param name="cd">        The ComponentDefinition we are writing from.</param>
    /// <param name="isResource">True if is resource, false if not.</param>
    private void WriteDartClass(ComponentDefinition cd, bool isResource)
    {
        string exportName;
        string typeName;

        if (cd.IsRootOfStructure)
        {
            WriteIndentedComment(cd.Structure.cgpComment());
            WriteIndentedComment(BuildCommentString(cd.Structure));

            // get the base type name from the root element
            typeName = cd.Structure.cgBaseTypeName(_dc, _primitiveTypeMap);
        }
        else
        {
            WriteIndentedComment(BuildCommentString(cd.Element));

            // get the base type name from the root element of this path
            typeName = cd.Element.cgBaseTypeName(_dc, false, _primitiveTypeMap);
        }

        if (
            string.IsNullOrEmpty(typeName)
            || (cd.Element.Path == "Element")
            || (cd.Element.Path == typeName)
        )
        {
            exportName = cd.cgNameRooted(NamingConvention.PascalCase);

            if (_genericsAndTypeHints.TryGetValue(cd.Element.Path, out GenericTypeHintInfo hint))
            {
                _writer.WriteLineIndented(
                    $"class" + $" {exportName}<{hint.Alias}> {{"
                );
            }
            else
            {
                _writer.WriteLineIndented($"class {exportName} {{");
            }
        }
        else
        {
            exportName = cd.cgNameRooted(NamingConvention.PascalCase);
            string exportTypeName = typeName.ToPascalCase();

            // if exportName is reserved, add a Fhir prefix
            if (ReservedWords.Contains(exportName))
            {
                exportName = $"Fhir{exportName}";
            }

            // if exportTypeName is reserved, add a Fhir prefix
            if (ReservedWords.Contains(exportTypeName))
            {
                exportTypeName = $"Fhir{exportTypeName}";
            }

            if (_genericsAndTypeHints.TryGetValue(cd.Element.Path, out GenericTypeHintInfo hint))
            {

                _writer.WriteLineIndented(
                    $"class"
                        + $" {exportName}"
                        + $" extends {exportTypeName} implements {hint.GenericHint} {{"
                );
            }
            else
            {
                if (ShouldWriteResourceType(cd.Structure.Name))
                {
                    _writer.WriteLineIndented($"class {exportName} extends {exportTypeName} implements FhirResource {{");
                }
                else
                {
                    _writer.WriteLineIndented($"class {exportName} extends {exportTypeName} {{");
                }
            }
        }

        _writer.IncreaseIndent();

        if (isResource)
        {
            if (ShouldWriteResourceType(cd.Structure.Name))
            {
                _exportedResources.Add(exportName);

                _writer.WriteLineIndented("/// Resource Type Name (for serialization) ");
                _writer.WriteLineIndented($"static const resourceType = '{cd.Structure.Name}';");
            }
            else
            {
                _writer.WriteLineIndented("/// Resource Type Name (for serialization) ");
                _writer.WriteLineIndented($"static const resourceType = '{cd.Structure.Name}';");
            }
        }

        // write elements
        WriteElements(cd, out List<ElementDefinition> elementsWithCodes);

        WriteConstructor(cd);

        WriteFromJsonMethod(cd);

        WriteToJsonMethod(cd);

        WriteCopyWithMethod(cd);

        _writer.DecreaseIndent();

        // close class (type)
        _writer.WriteLineIndented("}");

        if (_exportEnums)
        {
            foreach (ElementDefinition element in elementsWithCodes)
            {
                WriteCodes(element);
            }
        }
    }

    private void WriteCopyWithMethod(ComponentDefinition cd)
    {
        // empty line
        _writer.WriteLineIndented("");

        string name = cd.cgNameRooted(NamingConvention.PascalCase);
        if (ReservedWords.Contains(name))
        {
            name = $"Fhir{name}";
        }

        // add override if the class has inherited elements
        if (ComponentDefinitionHasInheritedElements(cd))
        {
            _writer.WriteLineIndented("@override");
        }

        _writer.WriteLineIndented($"{name} copyWith({{");
        _writer.IncreaseIndent();

        // Define parameters with nullable types for copyWith method
        foreach (ElementDefinition element in cd.Structure.cgElements(cd.Element.Path, true, false).OrderBy(e => e.Path))
        {
            Dictionary<string, string> values = NamesAndTypesForExport(element);
            foreach ((string exportName, string elementTypes) in values)
            {
                string propertyName = FhirSanitizationUtils.ToConvention(
                    exportName,
                    element.Path,
                    NamingConvention.CamelCase
                );

                if (ReservedWords.Contains(exportName))
                {
                    propertyName = $"fhir{exportName.Capitalize()}";
                }

                string nullableType = element.cgIsArray() ? $"List<{elementTypes}>?" : $"{elementTypes}?";

                if (_genericsAndTypeHints.TryGetValue(element.Path, out GenericTypeHintInfo hint))
                {
                    nullableType = $"{hint.Alias}?";
                }

                _writer.WriteLineIndented($"{nullableType} {propertyName},");
            }
        }

        _writer.DecreaseIndent();
        _writer.WriteLineIndented("}) {");
        _writer.IncreaseIndent();

        _writer.WriteLineIndented($"return {name}(");
        _writer.IncreaseIndent();

        // Assign each field, prioritizing the new value if provided
        foreach (ElementDefinition element in cd.Structure.cgElements(cd.Element.Path, true, false).OrderBy(e => e.Path))
        {
            Dictionary<string, string> values = NamesAndTypesForExport(element);
            foreach ((string exportName, string elementTypes) in values)
            {
                string propertyName = FhirSanitizationUtils.ToConvention(
                    exportName,
                    element.Path,
                    NamingConvention.CamelCase
                );

                if (ReservedWords.Contains(exportName))
                {
                    propertyName = $"fhir{exportName.Capitalize()}";
                }

                _writer.WriteLineIndented($"{propertyName}: {propertyName} ?? this.{propertyName},");
            }
        }

        _writer.DecreaseIndent();
        _writer.WriteLineIndented(");");

        _writer.DecreaseIndent();
        _writer.WriteLineIndented("}");

    }


    private void WriteConstructor(
        ComponentDefinition cd
    )
    {

        string className = cd.cgNameRooted(NamingConvention.PascalCase);
        if (ReservedWords.Contains(className))
        {
            className = $"Fhir{className}";
        }
        _writer.WriteLineIndented(
            $"{className}({{"
        );

        _writer.IncreaseIndent();

        foreach (ElementDefinition element in cd
                .Structure.cgElements(cd.Element.Path, true, false)
                .OrderBy(e => e.Path))
        {

            bool isOptional = element.cgIsOptional();
            bool isRequired = !isOptional;
            bool isInherited = element.cgIsInherited(cd.Structure);

            string requiredPrefix = isRequired ? "required " : string.Empty;
            string optionalSuffix = isOptional ? "?" : string.Empty;

            Dictionary<string, string> values = NamesAndTypesForExport(element);

            if (values.Count > 1 && !isOptional)
            {
                isOptional = true;
                requiredPrefix = "";
            }

            foreach ((string exportName, string elementTypes) in values)
            {
                string name = FhirSanitizationUtils.ToConvention(
                    exportName,
                    element.Path,
                    NamingConvention.CamelCase
                );

                if (ReservedWords.Contains(name))
                {
                    name = $"fhir{name.Capitalize()}";
                }

                if (isInherited)
                {
                    _writer.WriteLineIndented(
                     $"{requiredPrefix}super.{name},"
                  );
                }
                else
                {
                    _writer.WriteLineIndented(
                   $"{requiredPrefix}this.{name},"
                   );
                }
            }

        }

        _writer.DecreaseIndent();

        _writer.WriteLineIndented("});");
    }


    private void WriteFromJsonMethod(ComponentDefinition cd)
    {

        // empty line
        _writer.WriteLineIndented("");

        // if has any inherited elements, we need add @override
        if (ComponentDefinitionHasInheritedElements(cd))
        {
            _writer.WriteLineIndented("@override");
        }

        string name = cd.cgNameRooted(NamingConvention.PascalCase);
        if (ReservedWords.Contains(name))
        {
            name = $"Fhir{name}";
        }
        _writer.WriteLineIndented($"factory {name}.fromJson(Map<String, dynamic> json) {{");
        _writer.IncreaseIndent();



        _writer.WriteLineIndented($"return {name}(");
        _writer.IncreaseIndent();

        foreach (ElementDefinition element in cd.Structure.cgElements(cd.Element.Path, true, false).OrderBy(e => e.Path))
        {
            Dictionary<string, string> values = NamesAndTypesForExport(element);

            bool isOptional = element.cgIsOptional();
            bool isRequired = !isOptional;
            bool isInherited = element.cgIsInherited(cd.Structure);
            bool isArray = element.cgIsArray();



            if (values.Count > 1 && !isOptional)
            {
                isOptional = true;
            }



            foreach ((string exportName, string elementTypes) in values)
            {
                string jsonKey = exportName;

                string parsedValue;
                if (element.cgIsArray())
                {
                    if (FhirPrimitiveTypeMap.ContainsValue(elementTypes))
                    {
                        if (isOptional)
                        {
                            parsedValue = $"(json['{jsonKey}'] as List<dynamic>?)?.map((e) => e as {elementTypes}).toList()";
                        }
                        else
                        {
                            parsedValue = $"(json['{jsonKey}'] as List<dynamic>).map((e) => e as {elementTypes}).toList()";
                        }
                    }
                    else
                    {
                        if (isOptional)
                        {
                            parsedValue = $"(json['{jsonKey}'] as List<dynamic>?)?.map((e) => {elementTypes}.fromJson((e as Map).cast<String, dynamic>())).toList()";
                        }
                        else
                        {
                            parsedValue = $"(json['{jsonKey}'] as List<dynamic>).map((e) => {elementTypes}.fromJson((e as Map).cast<String, dynamic>())).toList()";
                        }
                    }
                }
                else if (FhirPrimitiveTypeMap.ContainsValue(elementTypes))
                {
                    if (isOptional)
                    {
                        parsedValue = $"json['{jsonKey}'] as {elementTypes}?";
                    }
                    else
                    {
                        parsedValue = $"json['{jsonKey}'] as {elementTypes}";
                    }
                }
                // check generics
                else if (_genericsAndTypeHints.TryGetValue(element.Path, out GenericTypeHintInfo hint))
                {
                    if (isOptional)
                    {
                        parsedValue = $"json['{jsonKey}'] != null ? {hint.Alias}.fromJson((json['{jsonKey}'] as Map).cast<String, dynamic>()) : null";
                    }
                    else
                    {
                        parsedValue = $"{hint.Alias}.fromJson((json['{jsonKey}'] as Map).cast<String, dynamic>())";
                    }
                }
                else
                {
                    if (isOptional)
                    {
                        parsedValue = $"json['{jsonKey}'] != null ? {elementTypes}.fromJson((json['{jsonKey}'] as Map).cast<String, dynamic>()) : null";
                    }
                    else
                    {
                        parsedValue = $"{elementTypes}.fromJson((json['{jsonKey}'] as Map).cast<String, dynamic>())";
                    }
                }

                string propertyName = FhirSanitizationUtils.ToConvention(
                    exportName,
                    element.Path,
                    NamingConvention.CamelCase
                );

                if (ReservedWords.Contains(exportName))
                {
                    propertyName = $"fhir{exportName.Capitalize()}";
                }

                _writer.WriteLineIndented($"{propertyName}: {parsedValue},");

            }
        }

        _writer.DecreaseIndent();
        _writer.WriteLineIndented(");");
        _writer.WriteLineIndented("}");


        _writer.DecreaseIndent();
    }

    private void WriteToJsonMethod(ComponentDefinition cd)
    {

        // empty line
        _writer.WriteLineIndented("");

        // if has any inherited elements, we need add @override
        if (ComponentDefinitionHasInheritedElements(cd))
        {
            _writer.WriteLineIndented("@override");
        }

        _writer.WriteLineIndented($"Map<String, dynamic> toJson() =>");
        _writer.IncreaseIndent();

        _writer.WriteLineIndented("{");
        _writer.IncreaseIndent();

        foreach (ElementDefinition element in cd.Structure.cgElements(cd.Element.Path, true, false).OrderBy(e => e.Path))
        {
            Dictionary<string, string> values = NamesAndTypesForExport(element);
            bool isOptional = element.cgIsOptional();
            bool isArray = element.cgIsArray();


            if (values.Count > 1 && !isOptional)
            {
                isOptional = true;
            }

            foreach ((string exportName, string elementTypes) in values)
            {
                string propertyName = FhirSanitizationUtils.ToConvention(
                    exportName,
                    element.Path,
                    NamingConvention.CamelCase
                );

                if (ReservedWords.Contains(exportName))
                {
                    propertyName = $"fhir{exportName.Capitalize()}";
                }

                string jsonKey = exportName;


                string value = $"{propertyName}";

                if (isArray)
                {
                    if (FhirPrimitiveTypeMap.ContainsValue(elementTypes))
                    {
                        if (isOptional)
                        {
                            value = $"{propertyName}?.map((e) => e).toList()";
                        }
                        else
                        {
                            value = $"{propertyName}.map((e) => e).toList()";
                        }
                    }
                    else
                    {
                        if (isOptional)
                        {
                            value = $"{propertyName}?.map((e) => e.toJson()).toList()";
                        }
                        else
                        {
                            value = $"{propertyName}.map((e) => e.toJson()).toList()";
                        }

                    }
                }
                else if (!FhirPrimitiveTypeMap.ContainsValue(elementTypes))
                {
                    if (isOptional)
                    {
                        value = $"{propertyName}?.toJson()";
                    }
                    else
                    {
                        value = $"{propertyName}.toJson()";
                    }
                }

                _writer.WriteLineIndented($"'{jsonKey}': {value},");
            }
        }

        _writer.DecreaseIndent();
        _writer.WriteLineIndented("};");

        _writer.DecreaseIndent();
    }

    private bool ComponentDefinitionHasInheritedElements(ComponentDefinition cd)
    {
        foreach (ElementDefinition element in cd.Structure.cgElements(cd.Element.Path, true, false).OrderBy(e => e.Path))
        {
            if (element.cgIsInherited(cd.Structure))
            {
                return true;
            }
        }
        return false;
    }



    /// <summary>Writes a code.</summary>
    /// <param name="element">The element.</param>
    private void WriteCodes(ElementDefinition element)
    {
        string codeName = FhirSanitizationUtils.ToConvention(
            $"{element.Path}.Codes",
            string.Empty,
            NamingConvention.PascalCase
        );

        if (codeName.Contains("[x]"))
        {
            codeName = codeName.Replace("[x]", string.Empty);
        }

        if (_exportedCodes.Contains(codeName))
        {
            return;
        }

        _exportedCodes.Add(codeName);

        _writer.WriteLineIndented($"/// Code Values for the {element.Path} field");

        _writer.WriteLineIndented($"enum {codeName} {{");

        _writer.IncreaseIndent();

        foreach (string code in element.cgCodes(_dc))
        {
            FhirSanitizationUtils.SanitizeForCode(
                code,
                _reservedWords,
                out string name,
                out string value
            );

            _writer.WriteLineIndented($"{name.ToUpperInvariant()} = \"{value}\",");
        }

        _writer.DecreaseIndent();

        _writer.WriteLineIndented("}");
    }

    /// <summary>Determine if we should write resource name.</summary>
    /// <param name="name">The name.</param>
    /// <returns>True if it succeeds, false if it fails.</returns>
    private static bool ShouldWriteResourceType(string name) =>
        name switch
        {
            "Resource" or "DomainResource" or "MetadataResource" or "CanonicalResource" => false,
            _ => true,
        };

    /// <summary>Writes the elements.</summary>
    /// <param name="cd">          The complex.</param>
    /// <param name="elementsWithCodes">[out] The elements with codes.</param>
    private void WriteElements(
        ComponentDefinition cd,
        out List<ElementDefinition> elementsWithCodes
    )
    {
        elementsWithCodes = [];

        foreach (
            ElementDefinition ed in cd
                .Structure.cgElements(cd.Element.Path, true, false)
                .Where(e => e.cgIsInherited(cd.Structure) == false)
                .OrderBy(e => e.Path)
        )
        {
            if (ed.cgIsInherited(cd.Structure))
            {
                continue;
            }

            WriteElement(ed);

            if (ed.cgHasCodes())
            {
                elementsWithCodes.Add(ed);
            }
        }
    }

    /// <summary>Names and types for export.</summary>
    /// <param name="ed">                    The ElementDefinition we are writing.</param>
    /// <param name="nameConvention">        (Optional) The name convention.</param>
    /// <param name="typeConvention">        (Optional) The type convention.</param>
    /// <param name="concatenatePath">       (Optional) True to concatenate path.</param>
    /// <param name="concatenationDelimiter">(Optional) The concatenation delimiter.</param>
    /// <param name="isComponent">           (Optional) True if is component, false if not.</param>
    /// <returns>A Dictionary&lt;string,string&gt;</returns>
    private Dictionary<string, string> NamesAndTypesForExport(
        ElementDefinition ed,
        NamingConvention nameConvention = NamingConvention.CamelCase,
        NamingConvention typeConvention = NamingConvention.PascalCase,
        bool concatenatePath = false,
        string concatenationDelimiter = ""
    )
    {

        Dictionary<string, string> values = [];

        string baseName = ed.cgName();
        bool isChoice = false;

        // check for a generic type hint override
        if (_genericsAndTypeHints.ContainsKey(ed.Path))
        {
            values.Add(
                FhirSanitizationUtils.ToConvention(
                    baseName,
                    ed.Path,
                    nameConvention,
                    concatenatePath,
                    concatenationDelimiter
                ),
                FhirSanitizationUtils.ToConvention(ed.Path, string.Empty, typeConvention)
            );

            return values;
        }

        IReadOnlyDictionary<string, ElementDefinition.TypeRefComponent> elementTypes = ed.cgTypes();

        if (elementTypes.Count == 0)
        {
            // check for a backbone element
            if (_dc.HasChildElements(ed.Path))
            {

                values.Add(
                    FhirSanitizationUtils.ToConvention(
                        baseName,
                        ed.Path,
                        nameConvention,
                        concatenatePath,
                        concatenationDelimiter
                    ),
                    FhirSanitizationUtils.ToConvention(ed.Path, string.Empty, typeConvention)
                );

                return values;
            }

            // if there are no types, use the base type
            values.Add(
                FhirSanitizationUtils.ToConvention(
                    baseName,
                    ed.Path,
                    nameConvention,
                    concatenatePath,
                    concatenationDelimiter
                ),
                FhirSanitizationUtils.ToConvention(
                    ed.cgBaseTypeName(_dc, true, _primitiveTypeMap),
                    string.Empty,
                    typeConvention
                )
            );

            return values;
        }

        if (baseName.Contains("[x]", StringComparison.OrdinalIgnoreCase))
        {

            baseName = baseName.Replace("[x]", string.Empty, StringComparison.OrdinalIgnoreCase);

            isChoice = true;
        }

        if (isChoice)
        {
            foreach (
                ElementDefinition.TypeRefComponent elementType in elementTypes.Values.OrderBy(et =>
                    et.Code
                )
            )
            {
                string name = FhirSanitizationUtils.ToConvention(
                    baseName,
                    ed.Path,
                    nameConvention,
                    concatenatePath,
                    concatenationDelimiter
                );


                string type;
                string combined;

                if (_primitiveTypeMap.TryGetValue(elementType.cgName(), out string? mapped))
                {
                    type = mapped;
                    combined = name + elementType.cgName().ToPascalCase();
                    // combined = name + type.ToPascalCase();
                }
                else
                {
                    type = FhirSanitizationUtils.ToConvention(
                        elementType.cgName(),
                        string.Empty,
                        typeConvention
                    );
                    combined = name + type;
                }

                _ = values.TryAdd(combined, type);
            }
        }
        else
        {
            string types = string.Empty;

            foreach (
                ElementDefinition.TypeRefComponent elementType in elementTypes.Values.OrderBy(et =>
                    et.Code
                )
            )
            {

                string type = _primitiveTypeMap.TryGetValue(
                    elementType.cgName(),
                    out string? mapped
                )
                    ? mapped
                    : elementType.cgName();

                if (string.IsNullOrEmpty(types))
                {
                    types = type;
                }
                else
                {
                    types += "|" + type;
                }
            }

            if ((types == "BackboneElement") || (types == "Element"))
            {

                // check for a backbone element
                if (_dc.HasChildElements(ed.Path))
                {
                    values.Add(
                        FhirSanitizationUtils.ToConvention(
                            baseName,
                            ed.Path,
                            nameConvention,
                            concatenatePath,
                            concatenationDelimiter
                        ),
                        FhirSanitizationUtils.ToConvention(ed.Path, string.Empty, typeConvention)
                    );

                    return values;
                }

                // if there are no types, use the base type
                values.Add(
                    FhirSanitizationUtils.ToConvention(
                        baseName,
                        ed.Path,
                        nameConvention,
                        concatenatePath,
                        concatenationDelimiter
                    ),
                    FhirSanitizationUtils.ToConvention(ed.Path, string.Empty, typeConvention)
                );

                return values;
            }

            string cased = FhirSanitizationUtils.ToConvention(
                baseName,
                string.Empty,
                nameConvention,
                concatenatePath,
                concatenationDelimiter
            );


            _ = values.TryAdd(cased, types);
        }

        return values;
    }

    private void WriteElement(ElementDefinition ed)
    {
        bool isOptional = ed.cgIsOptional();

        string optionalTypeSuffix = isOptional ? "?" : string.Empty;

        Dictionary<string, string> values = NamesAndTypesForExport(ed);

        // If there are multiple values and the element is not optional, make it optional
        if (values.Count > 1 && !isOptional)
        {
            isOptional = true;
            optionalTypeSuffix = "?";
        }

        foreach ((string exportName, string elementTypes) in values)
        {


            string actualExportName = exportName;
            // if exportName is reserved, add a Fhir prefix
            if (ReservedWords.Contains(exportName))
            {
                actualExportName = $"fhir{exportName.Capitalize()}";
            }
            // Write comments
            WriteIndentedComment(BuildCommentString(ed));

            IEnumerable<string> codes = ed.cgCodes(_dc);

            // Handle codes and value sets
            if (
                codes.Any()
                && !string.IsNullOrEmpty(ed.Binding.ValueSet)
                && !_unexportableSystems.Contains(_dc.UnversionedUrlForVs(ed.Binding.ValueSet))
            )
            {
                if (_exportEnums)
                {
                    // Use generated enum for codes
                    string codeName = $"{ed.Path}.Codes".ToPascalCase();
                    string typeDeclaration = ed.cgIsArray() ? $"List<{codeName}>" : codeName;
                    typeDeclaration += optionalTypeSuffix;

                    _writer.WriteLineIndented($"final {typeDeclaration} {actualExportName};");
                }
                else if (_dc.TryExpandVs(ed.Binding.ValueSet, out ValueSet? vs))
                {
                    // Use the full expansion
                    string codeValues = string.Join(
                        ", ",
                        vs.Expansion.Contains.Select(c => $"'{c.Code}'")
                    );
                    string typeDeclaration = ed.cgIsArray() ? "List<String>" : "String";
                    typeDeclaration += optionalTypeSuffix;

                    _writer.WriteLineIndented(
                        $"final {typeDeclaration} {actualExportName}; // Possible values: {codeValues}"
                    );
                }
                else
                {
                    // Inline the required codes
                    string codeValues = string.Join(", ", codes.Select(c => $"'{c}'"));
                    string typeDeclaration = ed.cgIsArray() ? "List<String>" : "String";
                    typeDeclaration += optionalTypeSuffix;

                    _writer.WriteLineIndented(
                        $"final {typeDeclaration} {actualExportName}; // Allowed codes: {codeValues}"
                    );
                }
            }
            else if (_genericsAndTypeHints.TryGetValue(ed.Path, out GenericTypeHintInfo typeHint))
            {
                // Handle generic type hints
                string typeDeclaration;

                typeDeclaration = typeHint.Alias;

                if (ed.cgIsArray())
                {
                    typeDeclaration = $"List<{typeDeclaration}>";
                }

                typeDeclaration += optionalTypeSuffix;

                _writer.WriteLineIndented($"final {typeDeclaration} {actualExportName};");
            }
            else if (elementTypes.Equals("Resource", StringComparison.Ordinal))
            {
                // Handle 'Resource' type
                string typeDeclaration = ed.cgIsArray() ? "List<Resource>" : "Resource";
                typeDeclaration += optionalTypeSuffix;

                _writer.WriteLineIndented($"final {typeDeclaration} {actualExportName};");
            }
            else
            {
                string typeDeclaration = ed.cgIsArray() ? $"List<{elementTypes}>" : elementTypes;

                // check if elementTypes is reserved
                if (ReservedWords.Contains(elementTypes))
                {
                    typeDeclaration = ed.cgIsArray() ? $"List<fhir{elementTypes.Capitalize()}>" : $"fhir{elementTypes.Capitalize()}";
                }
                // Default case
                typeDeclaration += optionalTypeSuffix;

                _writer.WriteLineIndented($"final {typeDeclaration} {actualExportName};");
            }



            // Handle primitive extensions if required
            if (RequiresPrimitiveExtension(elementTypes))
            {
                string extensionType = ed.cgIsArray() ? "List<Element>?" : "Element?";
                _writer.WriteLineIndented($"final {extensionType} _{actualExportName};");
            }
        }
    }

    /// <summary>Requires extension.</summary>
    /// <param name="typeName">Name of the type.</param>
    /// <returns>True if it succeeds, false if it fails.</returns>
    private static bool RequiresPrimitiveExtension(string typeName)
    {
        if (string.IsNullOrEmpty(typeName))
        {
            return false;
        }

        if (_primitiveTypeMap.ContainsKey(typeName))
        {
            return true;
        }

        return false;
    }

    /// <summary>Writes an indented comment.</summary>
    /// <param name="value">The value.</param>
    private void WriteIndentedComment(string value)
    {
        if (string.IsNullOrEmpty(value))
        {
            return;
        }

        string comment = value
            .Replace('\r', '\n')
            .Replace("\r\n", "\n", StringComparison.Ordinal)
            .Replace("\n\n", "\n", StringComparison.Ordinal);

        string[] lines = comment.Split('\n');
        foreach (string line in lines)
        {
            _writer.WriteIndented(" /// ");
            _writer.WriteLine(line);
        }

    }

    /// <summary>Information about written codes.</summary>
    private struct WrittenCodeInfo
    {
        internal string Name;
        internal string ConstName;
    }

    /// <summary>Information about the generic type hint.</summary>
    private struct GenericTypeHintInfo
    {
        internal string Alias;
        internal bool IncludeBase;
        internal string GenericHint;
    }

    private string ToLowerUnderscore(string input)
    {
        // Use a regular expression to insert an underscore before each capital letter (except the first one)
        string result = Regex.Replace(input, "(?<!^)([A-Z])", "_$1");

        // Convert the string to lowercase
        return result.ToLower();
    }
}
