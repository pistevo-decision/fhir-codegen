﻿// <copyright file="CSharpFirely2.cs" company="Microsoft Corporation">
//     Copyright (c) Microsoft Corporation. All rights reserved.
//     Licensed under the MIT License (MIT). See LICENSE in the repo root for license information.
// </copyright>


// <copyright file="Firely2.cs" company="Microsoft Corporation">
//     Copyright (c) Microsoft Corporation. All rights reserved.
//     Licensed under the MIT License (MIT). See LICENSE in the repo root for license information.
// </copyright>


using System.ComponentModel;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Text;
using System.Xml.Linq;
using Hl7.Fhir.Model;
using Hl7.Fhir.Utility;
using Microsoft.Health.Fhir.CodeGen.FhirExtensions;
using Microsoft.Health.Fhir.CodeGen.Models;
using Microsoft.Health.Fhir.CodeGen.Utils;
using Microsoft.Health.Fhir.CodeGenCommon.FhirExtensions;
using Microsoft.Health.Fhir.CodeGenCommon.Packaging;
using Ncqa.Cql.Model;
using static Microsoft.Health.Fhir.CodeGen.Lanugage.Firely.CSharpFirelyCommon;
using static Microsoft.Health.Fhir.CodeGenCommon.Extensions.FhirNameConventionExtensions;

namespace Microsoft.Health.Fhir.CodeGen.Lanugage.Firely;

public sealed class CSharpFirely2 : ILanguage<FirelyGenOptions>
{
    /// <summary>(Immutable) Name of the language.</summary>
    private const string _languageName = "CSharpFirely2";

    /// <summary>Gets the language name.</summary>
    public string Name => _languageName;

    /// <summary>Gets the FHIR primitive type map.</summary>
    public Dictionary<string, string> FhirPrimitiveTypeMap => CSharpFirelyCommon.PrimitiveTypeMap;

    /// <summary>Gets a value indicating whether this language is idempotent.</summary>
    public bool IsIdempotent => false;

    /// <summary>The namespace to use during export.</summary>
    private const string Namespace = "Hl7.Fhir.Model";

    /// <summary>FHIR information we are exporting.</summary>
    private DefinitionCollection _info = null!;

    /// <summary>Options for controlling the export.</summary>
    private FirelyGenOptions _options = null!;

    /// <summary>Keep track of information about written value sets.</summary>
    private Dictionary<string, WrittenValueSetInfo> _writtenValueSets = new();

    /// <summary>The split characters.</summary>
    private static readonly char[] _splitChars = { '|', ' ' };

    /// <summary>The currently in-use text writer.</summary>
    private ExportStreamWriter _writer = null!;

    /// <summary>The model writer.</summary>
    private ExportStreamWriter _modelWriter = null!;

    /// <summary>Pathname of the export directory.</summary>
    private string _exportDirectory = string.Empty;

    /// <summary>Structures to skip generating.</summary>
    private static readonly HashSet<string> _exclusionSet = new()
    {
        /* Since Base defines its methods abstractly, the pattern for generating it
            * is sufficiently different from derived classes that it makes sense not
            * to generate the methods (it's pretty empty too - no members on this abstract class) */
        "Base",

        /* PrimitiveType defines the magic `ObjectValue` member used by all derived
            * primitives to store their value. This makes the CopyTo(), IsExact() methods
            * different enough that it does not make sense to generate them. */
        "PrimitiveType",

        /* Element has the special `id` element, that is both an attribute in the
            * XML serialization and is not using a FHIR primitive for representation. Consequently,
            * the generated CopyTo() and IsExact() methods diverge too much to be useful. */
        "Element",

        /* Extension has the special `url` element, that is both an attribute in the
            * XML serialization and is not using a FHIR primitive for representation. Consequently,
            * the generated CopyTo() and IsExact() methods diverge too much to be useful. */
        "Extension",

        /* Narrative has a special `div` element, serialized as an element frm the
            * XHTML namespace, not using a normal FHIR primitive. This makes this class
            * deviate in ways we cannot achieve with the generator. */
        "Narrative",

        /* These two types are interfaces rather than classes (at least, for now)
            * so we're not generating them. Also, all types deriving from these
            * are generated to derive from DomainResource instead */
        "CanonicalResource",
        "MetadataResource",

        /* UCUM is used as a required binding in a codeable concept. Since we do not
            * use enums in this situation, it is not useful to generate this valueset
            */
        "http://hl7.org/fhir/ValueSet/ucum-units",
    };

    /// <summary>
    /// List of types introduced in R5 that are retrospectively introduced in R3 and R4.
    /// </summary>
    private static readonly List<WrittenModelInfo> _sharedR5DataTypes = new()
    {
        new WrittenModelInfo { CsName = "BackboneType", FhirName = "BackboneType", IsAbstract = true },
        new WrittenModelInfo { CsName = "Base", FhirName = "Base", IsAbstract = true },
        new WrittenModelInfo { CsName = "DataType", FhirName = "DataType", IsAbstract = true },
        new WrittenModelInfo { CsName = "PrimitiveType", FhirName = "PrimitiveType", IsAbstract = true },
    };

    /// <summary>
    /// List of complex datatype classes that are part of the 'base' subset. See <see cref="GenSubset"/>.
    /// </summary>
    private static readonly List<string> _baseSubsetComplexTypes = new()
    {
        "Attachment",
        "BackboneElement",
        "BackboneType",
        "Base",
        "CodeableConcept",
        "Coding",
        "ContactPoint",
        "ContactDetail",
        "DataType",
        "Element",
        "Extension",
        "Identifier",
        "Meta",
        "Narrative",
        "Period",
        "PrimitiveType",
        "Quantity",
        "Range",
        "Reference",
        "Signature",
        "UsageContext",
        "CodeableReference"
    };

    /// <summary>
    /// List of complex datatype classes that are part of the 'conformance' subset. See <see cref="GenSubset"/>.
    /// </summary>
    private static readonly List<string> _conformanceSubsetComplexTypes = new()
    {
        "ElementDefinition",
        "RelatedArtifact",
    };

    /// <summary>
    /// List of resource classes that are part of the 'base' subset. See <see cref="GenSubset"/>.
    /// </summary>
    private static readonly List<string> _baseSubsetResourceTypes = new()
    {
        "Binary",
        "Bundle",
        "DomainResource",
        "OperationOutcome",
        "Parameters",
        "Resource",
    };


    /// <summary>
    /// List of resource classes that are part of the 'conformance' subset. See <see cref="GenSubset"/>.
    /// </summary>
    private static readonly List<string> _conformanceSubsetResourceTypes = new()
    {
        "CapabilityStatement",
        "CodeSystem",
        "ElementDefinition",
        "StructureDefinition",
        "ValueSet",
    };

    /// <summary>
    /// List of all valuesets that we publish in the base subset
    /// </summary>
    private static readonly List<string> _baseSubsetValueSets = new()
    {
        "http://hl7.org/fhir/ValueSet/publication-status",
        "http://hl7.org/fhir/ValueSet/FHIR-version",

        // Doesn't strictly need to be in base (but in conformance),
        // but we used to generate it for base, so I am keeping it that way.
        "http://hl7.org/fhir/ValueSet/filter-operator",
    };

    /// <summary>
    /// List of all valuesets that we publish in the conformance subset.
    /// </summary>
    private static readonly List<string> _conformanceSubsetValueSets = new List<string>()
    {
        "http://hl7.org/fhir/ValueSet/capability-statement-kind",
        "http://hl7.org/fhir/ValueSet/binding-strength",
        "http://hl7.org/fhir/ValueSet/search-param-type",

        // These are necessary for CapabilityStatement/CapabilityStatement2
        // CapStat2 has disappeared in ballot, if that becomes final,
        // these don't have to be created as shared valuesets anymore.
        "http://hl7.org/fhir/ValueSet/restful-capability-mode",
        "http://hl7.org/fhir/ValueSet/type-restful-interaction",
        "http://hl7.org/fhir/ValueSet/system-restful-interaction",

        // For these valuesets the algorithm to determine whather a vs is shared
        // is still considering core extensions too. When this is corrected,
        // these can probably go.
        "http://hl7.org/fhir/ValueSet/constraint-severity",

        "http://hl7.org/fhir/ValueSet/codesystem-content-mode"
    };

    /// <summary>
    ///  List of all valuesets that we should publish as a shared Enum although there is only 1 reference to it.
    /// </summary>
    private static readonly List<(string, string)> _explicitSharedValueSets = new()
    {
        // This enum should go to Template-binding.cs because otherwise it will introduce a breaking change.
        ("R4", "http://hl7.org/fhir/ValueSet/messageheader-response-request"),
        ("R4", "http://hl7.org/fhir/ValueSet/concept-map-equivalence"),
        ("R4B", "http://hl7.org/fhir/ValueSet/messageheader-response-request"),
        ("R4B", "http://hl7.org/fhir/ValueSet/concept-map-equivalence"),
        ("R5", "http://hl7.org/fhir/ValueSet/constraint-severity"),
    };


    /// <summary>Gets the reserved words.</summary>
    /// <value>The reserved words.</value>
    private static readonly HashSet<string> _reservedWords = new HashSet<string>();

    private static readonly Func<WrittenModelInfo, bool> SupportedResourcesFilter = wmi => !wmi.IsAbstract;
    private static readonly Func<WrittenModelInfo, bool> FhirToCsFilter = wmi => !ExcludeFromCsToFhir!.Contains(wmi.FhirName);
    private static readonly Func<WrittenModelInfo, bool> CsToStringFilter = FhirToCsFilter;

    private static readonly string[] ExcludeFromCsToFhir =
    {
        "CanonicalResource",
        "MetadataResource",
    };

    /// <summary>
    /// The list of elements that would normally be represented using a CodeOfT enum, but that we
    /// want to be generated as a normal Code instead.
    /// </summary>
    private readonly List<string> _codedElementOverrides = new()
    {
        "CapabilityStatement.rest.resource.type"
    };

    /// <summary>
    /// Some valuesets have names that are the same as element names or are just not nice - use this collection
    /// to change the name of the generated enum as required.
    /// </summary>
    private readonly Dictionary<string, string> _enumNamesOverride = new()
    {
        ["http://hl7.org/fhir/ValueSet/characteristic-combination"] = "CharacteristicCombinationCode",
        ["http://hl7.org/fhir/ValueSet/claim-use"] = "ClaimUseCode",
        ["http://hl7.org/fhir/ValueSet/content-type"] = "ContentTypeCode",
        ["http://hl7.org/fhir/ValueSet/exposure-state"] = "ExposureStateCode",
        ["http://hl7.org/fhir/ValueSet/verificationresult-status"] = "StatusCode",
        ["http://terminology.hl7.org/ValueSet/v3-Confidentiality"] = "ConfidentialityCode",
        ["http://hl7.org/fhir/ValueSet/variable-type"] = "VariableTypeCode",
        ["http://hl7.org/fhir/ValueSet/group-measure"] = "GroupMeasureCode",
        ["http://hl7.org/fhir/ValueSet/coverage-kind"] = "CoverageKindCode",
        ["http://hl7.org/fhir/ValueSet/fhir-types"] = "FHIRAllTypes"
    };

    private record SinceVersion(FhirReleases.FhirSequenceCodes Since);

    private readonly Dictionary<string, string> _sinceAttributes = new()
    {
        ["Meta.source"] = "R4",
        ["Reference.type"] = "R4",
        ["Bundle.timestamp"] = "R4",
        ["Binary.data"] = "R4",
        ["ValueSet.compose.property"] = "R5",
        ["ValueSet.compose.include.copyright"] = "R5",
        ["ValueSet.expansion.property"] = "R5",
        ["ValueSet.expansion.contains.property"] = "R5",
        ["ValueSet.scope"] = "R5",
        ["Bundle.issues"] = "R5",
        ["CapabilityStatement.rest.resource.conditionalPatch"] = "R5",
        ["CapabilityStatement.versionAlgorithm[x]"] = "R5",
        ["CapabilityStatement.copyrightLabel"] = "R5",
        ["CapabilityStatement.acceptLanguage"] = "R5",
        ["CapabilityStatement.identifier"] = "R5",
        ["CodeSystem.concept.designation.additionalUse"] = "R5",
        ["CodeSystem.approvalDate"] = "R5",
        ["CodeSystem.lastReviewDate"] = "R5",
        ["CodeSystem.effectivePeriod"] = "R5",
        ["CodeSystem.topic"] = "R5",
        ["CodeSystem.author"] = "R5",
        ["CodeSystem.editor"] = "R5",
        ["CodeSystem.reviewer"] = "R5",
        ["CodeSystem.endorser"] = "R5",
        ["CodeSystem.relatedArtifact"] = "R5",
        ["CodeSystem.copyrightLabel"] = "R5",
        ["CodeSystem.versionAlgorithm[x]"] = "R5",
        ["ElementDefinition.constraint.suppress"] = "R5",
        ["ElementDefinition.mustHaveValue"] = "R5",
        ["ElementDefinition.valueAlternatives"] = "R5",
        ["ElementDefinition.obligation"] = "R5",
        ["ElementDefinition.obligation.code"] = "R5",
        ["ElementDefinition.obligation.actor"] = "R5",
        ["ElementDefinition.obligation.documentation"] = "R5",
        ["ElementDefinition.obligation.usage"] = "R5",
        ["ElementDefinition.obligation.filter"] = "R5",
        ["ElementDefinition.obligation.filterDocumentation"] = "R5",
        ["ElementDefinition.obligation.process"] = "R5",
        ["ElementDefinition.binding.additional"] = "R5",
        ["ElementDefinition.binding.additional.purpose"] = "R5",
        ["ElementDefinition.binding.additional.valueSet"] = "R5",
        ["ElementDefinition.binding.additional.documentation"] = "R5",
        ["ElementDefinition.binding.additional.shortDoco"] = "R5",
        ["ElementDefinition.binding.additional.usage"] = "R5",
        ["ElementDefinition.binding.additional.any"] = "R5",
        ["StructureDefinition.versionAlgorithm[x]"] = "R5",
        ["StructureDefinition.copyrightLabel"] = "R5",
        ["ValueSet.compose.include.concept.designation.additionalUse"] = "R5",
        ["ValueSet.expansion.next"] = "R5",
        ["ValueSet.expansion.contains.property.subProperty"] = "R5",
        ["ValueSet.expansion.contains.property.subProperty.code"] = "R5",
        ["ValueSet.expansion.contains.property.subProperty.value[x]"] = "R5",
        ["ValueSet.approvalDate"] = "R5",
        ["ValueSet.lastReviewDate"] = "R5",
        ["ValueSet.effectivePeriod"] = "R5",
        ["ValueSet.topic"] = "R5",
        ["ValueSet.author"] = "R5",
        ["ValueSet.editor"] = "R5",
        ["ValueSet.reviewer"] = "R5",
        ["ValueSet.endorser"] = "R5",
        ["ValueSet.relatedArtifact"] = "R5",
        ["ValueSet.copyrightLabel"] = "R5",
        ["ValueSet.versionAlgorithm[x]"] = "R5",
        ["Attachment.height"] = "R5",
        ["Attachment.width"] = "R5",
        ["Attachment.frames"] = "R5",
        ["Attachment.duration"] = "R5",
        ["Attachment.pages"] = "R5",
        ["RelatedArtifact.classifier"] = "R5",
        ["RelatedArtifact.resourceReference"] = "R5",
        ["RelatedArtifact.publicationStatus"] = "R5",
        ["RelatedArtifact.publicationDate"] = "R5",
        ["Signature.who"] = "R4",
        ["Signature.onBehalfOf"] = "R4",
        ["Signature.sigFormat"] = "R4",
        ["Signature.targetFormat"] = "R4"
    };

    private readonly Dictionary<string, (string since, string newName)> _untilAttributes = new()
    {
        ["Binary.content"] = ("R4", "Binary.data"),
        ["ElementDefinition.constraint.xpath"] = ("R5", ""),
        ["ValueSet.scope.focus"] = ("R5", ""),
        ["RelatedArtifact.url"] = ("R5", ""),
        ["Signature.blob"] = ("R4", "Signature.data"),
        ["Signature.contentType"] = ("R4", "")
    };

    /// <summary>True to export five ws.</summary>
    private bool _exportFiveWs = true;

    /// <summary>Gets the FHIR primitive type map.</summary>
    /// <value>The FHIR primitive type map.</value>
    Dictionary<string, string> ILanguage.FhirPrimitiveTypeMap => CSharpFirelyCommon.PrimitiveTypeMap;

    /// <summary>If a Cql ModelInfo is available, this will be the parsed XML model file.</summary>
    private Ncqa.Cql.Model.ModelInfo? _cqlModelInfo = null;
    private IDictionary<string, ClassInfo>? _cqlModelClassInfo = null;




    /// <summary>Export the passed FHIR version into the specified directory.</summary>
    /// <param name="info">           The information.</param>
    /// <param name="serverInfo">     Information describing the server.</param>
    /// <param name="options">        Options for controlling the operation.</param>
    /// <param name="exportDirectory">Directory to write files.</param>
    //void ILanguage.Export(
    //    FhirVersionInfo info,
    //    FhirCapabiltyStatement serverInfo,
    //    ExporterOptions options,
    //    string exportDirectory)
    public void Export(FirelyGenOptions options, DefinitionCollection info)
    {
        var subset = options.Subset;

        // STU3 satellite is a combination of satellite and conformance
        if ((info.FhirSequence == FhirReleases.FhirSequenceCodes.STU3) &&
            (subset == CSharpFirelyCommon.GenSubset.Satellite))
        {
            subset = CSharpFirelyCommon.GenSubset.Satellite | CSharpFirelyCommon.GenSubset.Conformance;
        }

        // only generate base definitions for R5
        if (subset.HasFlag(GenSubset.Base) && info.FhirSequence != FhirReleases.FhirSequenceCodes.R5)
        {
            Console.WriteLine($"Aborting {_languageName} for {info.FhirSequence}: code generation for the 'base' subset should be run on R5 only.");
            return;
        }

        // conformance subset is only valid for STU3 and R5
        if (subset.HasFlag(GenSubset.Conformance) &&
            (info.FhirSequence != FhirReleases.FhirSequenceCodes.STU3 &&
            info.FhirSequence != FhirReleases.FhirSequenceCodes.R5))
        {
            Console.WriteLine($"Aborting {_languageName} for {info.FhirSequence}: code generation for the 'conformance' subset should be run on STU3 or R5 only.");
            return;
        }

        _exportFiveWs = options.ExportFiveWs;

        // set internal vars so we don't pass them to every function
        _info = info;
        _options = options;
        _exportDirectory = options.OutputDirectory;
        _writtenValueSets = new Dictionary<string, WrittenValueSetInfo>();

        if (!Directory.Exists(_exportDirectory))
        {
            Directory.CreateDirectory(_exportDirectory);
        }

        if (!Directory.Exists(Path.Combine(_exportDirectory, "Generated")))
        {
            Directory.CreateDirectory(Path.Combine(_exportDirectory, "Generated"));
        }

        string cqlmodelResourceKey = options.CqlModel;
        if (!string.IsNullOrEmpty(cqlmodelResourceKey))
        {
            _cqlModelInfo = CqlModels.LoadEmbeddedResource(cqlmodelResourceKey);
            _cqlModelClassInfo = CqlModels.ClassesByName(_cqlModelInfo);
        }

        var allPrimitives = new Dictionary<string, WrittenModelInfo>();
        var allComplexTypes = new Dictionary<string, WrittenModelInfo>();
        var allResources = new Dictionary<string, WrittenModelInfo>();
        var dummy = new Dictionary<string, WrittenModelInfo>();

        string infoFilename = Path.Combine(_exportDirectory, "Generated", "_GeneratorLog.cs");

        // update the models for consisntency across different versions of FHIR
        ModifyDefinitionsForConsistency();

        using (var infoStream = new FileStream(infoFilename, FileMode.Create))
        using (var infoWriter = new ExportStreamWriter(infoStream))
        {
            _modelWriter = infoWriter;

            WriteGenerationComment(infoWriter);

            if (options.ExportStructures.Contains(CodeGenCommon.Models.FhirArtifactClassEnum.ValueSet))
            {
                WriteSharedValueSets(subset);
            }

            _modelWriter.WriteLineIndented("// Generated items");

            if (options.ExportStructures.Contains(CodeGenCommon.Models.FhirArtifactClassEnum.PrimitiveType))
            {
                WritePrimitiveTypes(_info.PrimitiveTypesByName.Values, ref dummy, subset);
            }

            AddModels(allPrimitives, _info.PrimitiveTypesByName.Values);

            if (options.ExportStructures.Contains(CodeGenCommon.Models.FhirArtifactClassEnum.ComplexType))
            {
                WriteComplexDataTypes(_info.ComplexTypesByName.Values, ref dummy, subset);
            }

            AddModels(allComplexTypes, _info.ComplexTypesByName.Values);
            AddModels(allComplexTypes, _sharedR5DataTypes);

            if (options.ExportStructures.Contains(CodeGenCommon.Models.FhirArtifactClassEnum.Resource))
            {
                WriteResources(_info.ResourcesByName.Values, ref dummy, subset);
            }

            AddModels(allResources, _info.ResourcesByName.Values);

            if (subset.HasFlag(GenSubset.Satellite))
            {
                WriteModelInfo(allPrimitives, allComplexTypes, allResources);
            }
        }
    }

    /// <summary>
    /// Modifies the definition structures for consistency.  Note that this makes the export *not* idempotent.
    /// </summary>
    private void ModifyDefinitionsForConsistency()
    {
        // We need to modify the (R4+-based) definition of Binary, to include
        // the pre-R4 element "content".
        if (_info.ResourcesByName.TryGetValue("Binary", out StructureDefinition? sdBinary))
        {
            if (!sdBinary.cgTryGetElementByPath("Binary.content", out _) &&
                sdBinary.cgTryGetElementByPath("Binary.data", out ElementDefinition? edData))
            {
                // make a copy of the data element
                ElementDefinition edContent = (ElementDefinition)edData.DeepCopy();

                // update the copied element to be the content element
                edContent.ElementId = "Binary.content";
                edContent.Path = "Binary.content";
                edContent.Min = 1;
                edContent.Max = "1";

                // add the copied element to the binary structure, note that the DefinitionCollection needs to process and track
                sdBinary.cgInsertElement(_info, edContent, edData.cgFieldOrder(), false);
            }
        }

        // We need to modify the definition of Signature, to include
        // the STU3 content.
        if (_info.ComplexTypesByName.TryGetValue("Signature", out StructureDefinition? sdSignature))
        {
            if (!sdSignature.cgTryGetElementByPath("Signature.blob", out _) &&
                sdSignature.cgTryGetElementByPath("Signature.data", out ElementDefinition? edData))
            {
                // make a copy of the data element
                ElementDefinition edBlob = (ElementDefinition)edData.DeepCopy();

                // update the copied element to be the blob element
                edBlob.ElementId = "Signature.blob";
                edBlob.Path = "Signature.blob";
                edBlob.Min = 0;
                edBlob.Max = "1";

                // add the copied element to the signature structure, note that the DefinitionCollection needs to process and track
                sdSignature.cgInsertElement(_info, edBlob, edData.cgFieldOrder(), false);
            }

            if (!sdSignature.cgTryGetElementByPath("Signature.contentType", out _))
            {
                // create a new element for the contentType (values pulled from STU3)
                ElementDefinition edContentType = new()
                {
                    ElementId = "Signature.contentType",
                    Path = "Signature.contentType",
                    Short = "The technical format of the signature",
                    Definition = "A mime type that indicates the technical format of the signature. Important mime types are application/signature+xml for X ML DigSig, application/jwt for JWT, and image/* for a graphical image of a signature, etc.",
                    Min = 0,
                    Max = "1",
                    Type = new() { new() { Code = "code" } },
                    IsSummary = true,
                    Binding = new()
                    {
                        Strength = Hl7.Fhir.Model.BindingStrength.Required,
                        ValueSet = new Canonical("http://www.rfc-editor.org/bcp/bcp13.txt"),
                        Description = "The mime type of an attachment. Any valid mime type is allowed.",
                        Extension = new List<Extension>
                        {
                            new()
                            {
                                Url = CommonDefinitions.ExtUrlBindingName,
                                Value = new FhirString("MimeType"),
                            },
                            new()
                            {
                                Url = CommonDefinitions.ExtUrlIsCommonBinding,
                                Value = new FhirBoolean(true),
                            }
                        }
                    }
                };

                sdSignature.cgInsertElement(_info, edContentType, 6, false);
            }

            if (sdSignature.cgTryGetElementById("Signature.who", out ElementDefinition? edWho))
            {
                // make it a choice type by adding uri, like it was in STU3
                edWho.ElementId = "Signature.who[x]";
                edWho.Path = "Signature.who[x]";
                edWho.Type.Add(new() { Code = "uri" });
            }

            if (sdSignature.cgTryGetElementById("Signature.onBehalfOf", out ElementDefinition? edOnBehalfOf))
            {
                // make it a choice type by adding uri, like it was in STU3
                edOnBehalfOf.ElementId = "Signature.onBehalfOf[x]";
                edOnBehalfOf.Path = "Signature.onBehalfOf[x]";
                edOnBehalfOf.Type.Add(new() { Code = "uri" });
            }
        }

        // Element ValueSet.scope.focus has been removed in R5 (5.0.0-snapshot3). Adding this element to the list of Resources,
        // so we can add a [NotMapped] attribute later.
        if (_info.ResourcesByName.TryGetValue("ValueSet", out StructureDefinition? sdValueSet) &&
            sdValueSet.cgTryGetElementById("ValueSet.scope", out _) &&
            !sdValueSet.cgTryGetElementById("ValueSet.scope.focus", out _))
        {
            // create a new element for the focus (values pulled from 5.0.0-snapshot1)
            ElementDefinition edFocus = new()
            {
                ElementId = "ValueSet.scope.focus",
                Path = "ValueSet.scope.focus",
                Short = "General focus of the Value Set as it relates to the intended semantic space",
                Definition = "The general focus of the Value Set as it relates to the intended semantic space. This can be the information about clinical relevancy or the statement about the general focus of the Value Set, such as a description of types of messages, payment options, geographic locations, etc.",
                Min = 0,
                Max = "1",
                Type = new() { new() { Code = "string" } },
                Constraint = new()
                {
                    new()
                    {
                        Key = "ele-1",
                        Severity = ConstraintSeverity.Error,
                        Human = "All FHIR elements must have a @value or children",
                        Expression = "hasValue() or (children().count() > id.count())",
                    },
                },
                IsSummary = false,
                IsModifier = false,
                MustSupport = false,
            };

            sdValueSet.cgInsertElement(_info, edFocus, 3, false);
        }

        // Element Bundle.link.relation changed from FhirString to Code<Hl7.Fhir.Model.Bundle.LinkRelationTypes> in R5 (5.0.0-snapshot3).
        // We decided to leave the type to FhirString
        if (_info.ResourcesByName.TryGetValue("Bundle", out StructureDefinition? sdBundle) &&
            sdBundle.cgTryGetElementById("Bundle.link.relation", out ElementDefinition? edRelation))
        {
            edRelation.Type = new() { new() { Code = "string" } };
        }

        // Element ElementDefinition.constraint.xpath has been removed in R5 (5.0.0-snapshot3). Adding this element to the list of ComplexTypes,
        // so we can add a [NotMapped] attribute later.
        if (_info.ComplexTypesByName.TryGetValue("ElementDefinition", out StructureDefinition? sdElementDefinition) &&
            sdElementDefinition.cgTryGetElementById("ElementDefinition.constraint", out _) &&
            !sdElementDefinition.cgTryGetElementById("ElementDefinition.constraint.xpath", out _))
        {
            // create a new element for the xpath (values pulled from 5.0.0-snapshot1)
            ElementDefinition edXPath = new()
            {
                ElementId = "ElementDefinition.constraint.xpath",
                Path = "ElementDefinition.constraint.xpath",
                Short = "XPath expression of constraint",
                Definition = "An XPath expression of constraint that can be executed to see if this constraint is met.",
                Min = 0,
                Max = "1",
                Type = new() { new() { Code = "string" } },
                IsSummary = true,
            };

            sdElementDefinition.cgInsertElement(_info, edXPath, 7, false);
        }

        // We need to modify the (R4+-based) definition of RelatedArtifact, to include
        // the pre-R4 element "url".
        if (_info.ResourcesByName.TryGetValue("RelatedArtifact", out StructureDefinition? sdRelatedArtifact) &&
            !sdRelatedArtifact.cgTryGetElementById("RelatedArtifact.url", out _))
        {
            // create a new element for the url (values pulled from STU3)
            ElementDefinition edUrl = new()
            {
                ElementId = "RelatedArtifact.url",
                Path = "RelatedArtifact.url",
                Short = "Where the artifact can be accessed",
                Definition = "A url for the artifact that can be followed to access the actual content.",
                Comment = "If a document or resource element is present, this element SHALL NOT be provided (use the url or reference in the Attachment or resource reference).",
                Min = 0,
                Max = "1",
                Type = new() { new() { Code = "uri" } },
                IsSummary = true,
            };

            sdRelatedArtifact.cgInsertElement(_info, edUrl, 6, false);
        }
    }

    /// <summary>Writes a model information.</summary>
    /// <param name="writtenPrimitives">   The written primitives.</param>
    /// <param name="writtenComplexTypes">List of types of the written complexes.</param>
    /// <param name="writtenResources">   The written resources.</param>
    private void WriteModelInfo(
        Dictionary<string, WrittenModelInfo> writtenPrimitives,
        Dictionary<string, WrittenModelInfo> writtenComplexTypes,
        Dictionary<string, WrittenModelInfo> writtenResources)
    {
        string filename = Path.Combine(_exportDirectory, "Generated", "Template-ModelInfo.cs");

        using (FileStream stream = new FileStream(filename, FileMode.Create))
        using (ExportStreamWriter writer = new ExportStreamWriter(stream))
        {
            _writer = writer;

            WriteGenerationComment();

            _writer.WriteLineIndented("using System;");
            _writer.WriteLineIndented("using System.Collections.Generic;");
            _writer.WriteLineIndented("using Hl7.Fhir.Introspection;");
            _writer.WriteLineIndented("using Hl7.Fhir.Validation;");
            _writer.WriteLineIndented("using System.Linq;");
            _writer.WriteLineIndented("using System.Runtime.Serialization;");
            _writer.WriteLine(string.Empty);

            WriteCopyright();

            WriteNamespaceOpen();

            WriteIndentedComment(
                "A class with methods to retrieve information about the\n" +
                "FHIR definitions based on which this assembly was generated.");

            _writer.WriteLineIndented("public partial class ModelInfo");

            // open class
            OpenScope();

            WriteSupportedResources(writtenResources.Values.Where(SupportedResourcesFilter));

            WriteFhirVersion();

            WriteFhirToCs(writtenPrimitives.Values.Where(FhirToCsFilter), writtenComplexTypes.Values.Where(FhirToCsFilter), writtenResources.Values.Where(FhirToCsFilter));
            WriteCsToString(writtenPrimitives.Values.Where(CsToStringFilter), writtenComplexTypes.Values.Where(CsToStringFilter), writtenResources.Values.Where(CsToStringFilter));

            WriteSearchParameters();

            // close class
            CloseScope();

            WriteNamespaceClose();
        }
    }

    /// <summary>Writes the search parameters.</summary>
    private void WriteSearchParameters()
    {
        _writer.WriteLineIndented("public static List<SearchParamDefinition> SearchParameters = new List<SearchParamDefinition>()");
        OpenScope();

        foreach (StructureDefinition complex in _info.ResourcesByName.Values)
        {
            IReadOnlyDictionary<string, SearchParameter> resourceSearchParams = _info.SearchParametersForBase(complex.Name);
            if (!resourceSearchParams.Any())
            {
                continue;
            }

            foreach (SearchParameter sp in resourceSearchParams.Values.OrderBy(s => s.Name))
            {
                if (sp.Experimental == true)
                {
                    continue;
                }

                string description;

                if ((!string.IsNullOrEmpty(sp.Description)) &&
                    sp.Description.StartsWith("Multiple", StringComparison.Ordinal))
                {
                    description = string.Empty;
                }
                else
                {
                    description = sp.Description;
                }

                string searchType = sp.Type == null
                    ? string.Empty
                    : FhirSanitizationUtils.SanitizedToConvention(sp.Type.GetLiteral()!, NamingConvention.PascalCase);

                string path = sp.cgXPath();
                if (!string.IsNullOrEmpty(path))
                {
                    string temp = path
                        .Replace("f:", string.Empty, StringComparison.Ordinal)
                        .Replace('/', '.')
                        .Replace('(', '[')
                        .Replace(')', ']');

                    IEnumerable<string> split = temp
                        .Split(_splitChars, StringSplitOptions.RemoveEmptyEntries)
                        .Where(s => s.StartsWith(complex.Name + ".", StringComparison.Ordinal));

                    path = "\"" + string.Join("\", \"", split) + "\", ";
                }

                string target;

                if (!sp.Target.Any())
                {
                    target = string.Empty;
                }
                else
                {
                    SortedSet<string> sc = new SortedSet<string>();

                    foreach (string t in sp.Target.Select(t => t.GetLiteral()!))
                    {
                        sc.Add("ResourceType." + t);
                    }

                    // HACK: for http://hl7.org/fhir/SearchParameter/clinical-encounter,
                    // none of the base resources have EpisodeOfCare as target, except
                    // Procedure and DeviceRequest. There is no way you can see this from the
                    // source data we generate this from, afaik, so we need to make
                    // a special case here.
                    // Brian P reported that there are many such exceptions - but this one
                    // was reported as a bug. Again, there is no way to know this from our
                    // inputs, so this will remain manually maintained input.
                    if (sp.Id == "clinical-encounter")
                    {
                        if (_info.FhirSequence == FhirReleases.FhirSequenceCodes.STU3)
                        {
                            if (complex.Name != "Procedure" && complex.Name != "DeviceRequest")
                            {
                                sc.Remove("ResourceType.EpisodeOfCare");
                            }
                        }
                        else
                        {
                            if (complex.Name != "DocumentReference")
                            {
                                sc.Remove("ResourceType.EpisodeOfCare");
                            }
                        }
                    }

                    target = ", Target = new ResourceType[] { " + string.Join(", ", sc) + ", }";
                }

                string xpath = string.IsNullOrEmpty(sp.cgXPath()) ? string.Empty : ", XPath = \"" + sp.cgXPath() + "\"";
                string expression = string.IsNullOrEmpty(sp.Expression) ? string.Empty : ", Expression = \"" + sp.Expression + "\"";
                string urlComponent = $", Url = \"{sp.Url}\"";

                string[] components = sp.Component?.Select(c => $"""new SearchParamComponent("{c.Definition}", "{c.Expression}")""").ToArray() ?? Array.Empty<string>();
                string strComponents = (components.Length > 0) ? $", Component = new SearchParamComponent[] {{ {string.Join(',', components)} }}" : string.Empty;

                _writer.WriteLineIndented(
                    $"new SearchParamDefinition() " +
                        $"{{" +
                        $" Resource = \"{complex.Name}\"," +
                        $" Name = \"{sp.Name}\"," +
                        $" Code = \"{sp.Code}\"," +
                        (_info.FhirSequence == FhirReleases.FhirSequenceCodes.STU3 ?
                            $" Description = @\"{SanitizeForMarkdown(description)}\"," :
                            $" Description = new Markdown(@\"{SanitizeForMarkdown(description)}\"),") +
                        $" Type = SearchParamType.{searchType}," +
                        $" Path = new string[] {{ {path}}}" +
                        target +
                        xpath +
                        expression +
                        urlComponent +
                        strComponents +
                        $" }},");
            }
        }

        CloseScope(true);
    }

    /// <summary>Sanitize for markdown.</summary>
    /// <param name="value">The value.</param>
    private static string SanitizeForMarkdown(string value)
    {
        if (string.IsNullOrEmpty(value))
        {
            return string.Empty;
        }

#pragma warning disable CA1307 // Specify StringComparison
        return value.Replace("\"", "\"\"").Replace("\r", @"\r").Replace("\n", @"\n");
#pragma warning restore CA1307 // Specify StringComparison
    }

    /// <summary>Writes the C# to FHIR map dictionary.</summary>
    /// <param name="writtenPrimitives">  The written primitives.</param>
    /// <param name="writtenComplexTypes">List of types of the written complexes.</param>
    /// <param name="writtenResources">   The written resources.</param>
    private void WriteCsToString(
        IEnumerable<WrittenModelInfo> writtenPrimitives,
        IEnumerable<WrittenModelInfo> writtenComplexTypes,
        IEnumerable<WrittenModelInfo> writtenResources)
    {
        _writer.WriteLineIndented("public static Dictionary<Type,string> FhirCsTypeToString = new Dictionary<Type,string>()");
        OpenScope();

        foreach (WrittenModelInfo type in writtenPrimitives.Concat(writtenComplexTypes).OrderBy(t => t.FhirName))
        {
            _writer.WriteLineIndented($"{{ typeof({type.CsName}), \"{type.FhirName}\" }},");
        }

        _writer.WriteLine(string.Empty);

        foreach (WrittenModelInfo type in writtenResources.OrderBy(t => t.FhirName))
        {
            _writer.WriteLineIndented($"{{ typeof({type.CsName}), \"{type.FhirName}\" }},");
        }

        CloseScope(true);
    }

    /// <summary>Writes the FHIR to C# map dictionary.</summary>
    /// <param name="writtenPrimitives">  The written primitives.</param>
    /// <param name="writtenComplexTypes">List of types of the written complexes.</param>
    /// <param name="writtenResources">   The written resources.</param>
    private void WriteFhirToCs(
        IEnumerable<WrittenModelInfo> writtenPrimitives,
        IEnumerable<WrittenModelInfo> writtenComplexTypes,
        IEnumerable<WrittenModelInfo> writtenResources)
    {
        _writer.WriteLineIndented("public static Dictionary<string,Type> FhirTypeToCsType = new Dictionary<string,Type>()");
        OpenScope();

        foreach (WrittenModelInfo type in writtenPrimitives.Concat(writtenComplexTypes).OrderBy(t => t.FhirName))
        {
            _writer.WriteLineIndented($"{{ \"{type.FhirName}\", typeof({type.CsName}) }},");
        }

        _writer.WriteLine(string.Empty);

        foreach (WrittenModelInfo type in writtenResources.OrderBy(t => t.FhirName))
        {
            _writer.WriteLineIndented($"{{ \"{type.FhirName}\", typeof({type.CsName}) }},");
        }

        CloseScope(true);
    }

    /// <summary>Writes the FHIR version.</summary>
    private void WriteFhirVersion()
    {
        _writer.WriteLineIndented("public static string Version");
        OpenScope();
        _writer.WriteLineIndented($"get {{ return \"{_info.FhirVersionLiteral}\"; }}");
        CloseScope();
    }

    /// <summary>Writes the supported resources dictionary.</summary>
    /// <param name="resources">The written resources.</param>
    private void WriteSupportedResources(IEnumerable<WrittenModelInfo> resources)
    {
        _writer.WriteLineIndented("public static List<string> SupportedResources = new List<string>()");
        OpenScope();

        foreach (WrittenModelInfo wmi in resources.OrderBy(s => s.FhirName))
        {
            _writer.WriteLineIndented($"\"{wmi.FhirName}\",");
        }

        CloseScope(true);
    }

    /// <summary>Writes the shared enums.</summary>
    private void WriteSharedValueSets(GenSubset subset)
    {
        HashSet<string> usedEnumNames = new HashSet<string>();

        string filename = Path.Combine(_exportDirectory, "Generated", "Template-Bindings.cs");

        using (FileStream stream = new FileStream(filename, FileMode.Create))
        using (ExportStreamWriter writer = new ExportStreamWriter(stream))
        {
            _writer = writer;

            WriteHeaderBasic();
            WriteNamespaceOpen();

            // traverse all versions of all value sets
            foreach ((string unversionedUrl, string[] versions) in _info.ValueSetVersions.OrderBy(kvp => kvp.Key))
            {
                if (_exclusionSet.Contains(unversionedUrl))
                {
                    continue;
                }

                // traverse value sets starting with highest version
                foreach (string vsVersion in versions.OrderDescending())
                {
                    if (!_info.TryGetValueSet(unversionedUrl, vsVersion, out ValueSet? vs))
                    {
                        continue;
                    }

                    IReadOnlyDictionary<string, ElementDefinition[]> coreBindings = _info.CoreBindingsForVs(vs.Url);
                    Hl7.Fhir.Model.BindingStrength? strongestBinding = _info.StrongestBinding(coreBindings);

                    if (strongestBinding != Hl7.Fhir.Model.BindingStrength.Required)
                    {
                        /* Since required bindings cannot be extended, those are the only bindings that
                           can be represented using enums in the POCO classes (using <c>Code&lt;T&gt;</c>). All other coded members
                           use <c>Code</c>, <c>Coding</c> or <c>CodeableConcept</c>.
                           Consequently, we only need to generate enums for valuesets that are used as
                           required bindings anywhere in the datamodel. */
                        continue;
                    }

                    IEnumerable<string> referencedBy = coreBindings.Values.SelectMany(v => v).cgExtractBaseTypes();

                    if ((referencedBy.Count() < 2) && !_explicitSharedValueSets.Contains((_info.FhirSequence.ToString(), vs.Url)))
                    {
                        /* ValueSets that are used in a single POCO are generated as a nested enum inside that
                         * POCO, not here in the shared valuesets */

                        continue;
                    }


                    // If this is a shared valueset that will be generated in the base or conformance subset,
                    // don't also generate it here.
                    bool writeValueSet =
                        (subset.HasFlag(GenSubset.Satellite) && !(_baseSubsetValueSets.Contains(vs.Url) || _conformanceSubsetValueSets.Contains(vs.Url)))
                        || subset.HasFlag(GenSubset.Conformance) && _conformanceSubsetValueSets.Contains(vs.Url)
                        || subset.HasFlag(GenSubset.Base) && _baseSubsetValueSets.Contains(vs.Url);

                    WriteEnum(vs, string.Empty, usedEnumNames, silent: !writeValueSet);

                    if (writeValueSet)
                    {
                        _modelWriter.WriteLineIndented($"// Generated Shared Enumeration: {_writtenValueSets[vs.Url].ValueSetName} ({vs.Url})");
                    }
                    else
                    {
                        _modelWriter.WriteLineIndented($"// Deferred generation of Shared Enumeration (will be generated in another subset): {_writtenValueSets[vs.Url].ValueSetName} ({vs.Url})");
                    }

                    _modelWriter.IncreaseIndent();

                    foreach (string path in coreBindings.Values.SelectMany(v => v.Select(e => e.Path)).OrderDescending())
                    {
                        string name = path.Split('.')[0];

                        if (_info.ComplexTypesByName.ContainsKey(name))
                        {
                            _modelWriter.WriteLineIndented($"// Used in model class (type): {path}");
                            continue;
                        }

                        _modelWriter.WriteLineIndented($"// Used in model class (resource): {path}");
                    }

                    _modelWriter.DecreaseIndent();
                    _modelWriter.WriteLine(string.Empty);
                }
            }

            WriteNamespaceClose();
        }
    }

    /// <summary>Writes the complex data types.</summary>
    /// <param name="complexes">    The complex data types.</param>
    /// <param name="writtenModels">[in,out] The written models.</param>
    /// <param name="subset"></param>
    private void WriteResources(
        IEnumerable<StructureDefinition> complexes,
        ref Dictionary<string, WrittenModelInfo> writtenModels,
        GenSubset subset)
    {
        foreach (StructureDefinition complex in complexes.OrderBy(c => c.Name))
        {
            if (_exclusionSet.Contains(complex.Name))
            {
                continue;
            }

            if ((subset.HasFlag(GenSubset.Base) && _baseSubsetResourceTypes.Contains(complex.Name)) ||
                (subset.HasFlag(GenSubset.Conformance) && _conformanceSubsetResourceTypes.Contains(complex.Name)) ||
                (subset.HasFlag(GenSubset.Satellite) && !_baseSubsetResourceTypes.Concat(_conformanceSubsetResourceTypes).Contains(complex.Name)))
            {
                WriteResource(complex, ref writtenModels, subset);
            }
        }
    }

    /// <summary>Writes a complex data type.</summary>
    /// <param name="complex">      The complex data type.</param>
    /// <param name="writtenModels">[in,out] The written models.</param>
    /// <param name="subset"></param>
    private void WriteResource(
        StructureDefinition complex,
        ref Dictionary<string, WrittenModelInfo> writtenModels,
        GenSubset subset)
    {
        string exportName = complex.Name.ToPascalCase();

        writtenModels.Add(
            complex.Name,
            new WrittenModelInfo()
            {
                FhirName = complex.Name,
                CsName = $"{Namespace}.{exportName}",
                IsAbstract = complex.Abstract == true,
            });

        string filename = Path.Combine(_exportDirectory, "Generated", $"{exportName}.cs");

        _modelWriter.WriteLineIndented($"// {exportName}.cs");

        using (FileStream stream = new FileStream(filename, FileMode.Create))
        using (ExportStreamWriter writer = new ExportStreamWriter(stream))
        {
            _writer = writer;

            WriteHeaderComplexDataType();

            WriteNamespaceOpen();

            WriteComponent(complex.cgComponent(), exportName, true, 0, subset);

            WriteNamespaceClose();

            WriteFooter();
        }
    }

    /// <summary>Writes the complex data types.</summary>
    /// <param name="complexes">    The complex data types.</param>
    /// <param name="writtenModels">[in,out] The written models.</param>
    /// <param name="subset"></param>
    private void WriteComplexDataTypes(
        IEnumerable<StructureDefinition> complexes,
        ref Dictionary<string, WrittenModelInfo> writtenModels,
        GenSubset subset)
    {
        foreach (StructureDefinition complex in complexes.OrderBy(c => c.Name))
        {
            if (_exclusionSet.Contains(complex.Name))
            {
                continue;
            }

            if ((subset.HasFlag(GenSubset.Base) && _baseSubsetComplexTypes.Contains(complex.Name)) ||
                (subset.HasFlag(GenSubset.Conformance) && _conformanceSubsetComplexTypes.Contains(complex.Name)) ||
                (subset.HasFlag(GenSubset.Satellite) && !_baseSubsetComplexTypes.Concat(_conformanceSubsetComplexTypes).Contains(complex.Name)))
            {
                WriteComplexDataType(complex, ref writtenModels, subset);
            }
        }
    }

    /// <summary>Writes a complex data type.</summary>
    /// <param name="complex">      The complex data type.</param>
    /// <param name="writtenModels">[in,out] The written models.</param>
    /// <param name="subset"></param>
    private void WriteComplexDataType(
        StructureDefinition complex,
        ref Dictionary<string, WrittenModelInfo> writtenModels,
        GenSubset subset)
    {
        string exportName = complex.Name.ToPascalCase();

        if (CSharpFirelyCommon.TypeNameMappings.ContainsKey(exportName))
        {
            exportName = CSharpFirelyCommon.TypeNameMappings[exportName];
        }

        writtenModels.Add(
            complex.Name,
            new WrittenModelInfo()
            {
                FhirName = complex.Name,
                CsName = $"{Namespace}.{exportName}",
                IsAbstract = complex.Abstract == true,
            });

        string filename = Path.Combine(_exportDirectory, "Generated", $"{exportName}.cs");

        _modelWriter.WriteLineIndented($"// {exportName}.cs");

        using (FileStream stream = new FileStream(filename, FileMode.Create))
        using (ExportStreamWriter writer = new ExportStreamWriter(stream))
        {
            _writer = writer;

            WriteHeaderComplexDataType();

            WriteNamespaceOpen();

            WriteComponent(complex.cgComponent(), exportName, false, 0, subset);

            WriteNamespaceClose();

            WriteFooter();
        }
    }

    /// <summary>Writes a component.</summary>
    /// <param name="complex">              The complex data type.</param>
    /// <param name="exportName">           Name of the export.</param>
    /// <param name="isResource">           True if is resource, false if not.</param>
    /// <param name="depth">                The depth.</param>
    /// <param name="subset"></param>
    private void WriteComponent(
        ComponentDefinition complex,
        string exportName,
        bool isResource,
        int depth,
        GenSubset subset)
    {
        string complexName = complex.cgName();
        bool isAbstract = complex.Structure.Abstract == true;

        List<WrittenElementInfo> exportedElements = new List<WrittenElementInfo>();

        WriteIndentedComment($"{complex.Element.Short}");

        WriteSerializable();

        string fhirTypeConstructor = $"\"{complexName}\",\"{complex.cgUrl()}\"";

        if (isResource)
        {
            _writer.WriteLineIndented($"[FhirType({fhirTypeConstructor}, IsResource=true)]");
        }
        else
        {
            _writer.WriteLineIndented($"[FhirType({fhirTypeConstructor})]");
        }

        var isPatientClass = false;

        if (complex.cgBaseTypeName(_info) == "Quantity")
        {
            // Constrained quantities are handled differently
            WriteConstrainedQuantity(complex, exportName);
            return;
        }

        string abstractFlag = isAbstract ? " abstract" : string.Empty;
        List<string> interfaces = new List<string>();

        if (_cqlModelInfo?.patientClassName is not null)
        {
            // Just skip the model alias, I am currently not bothered enough to be more precise
            var className = _cqlModelInfo.patientClassName.Split('.')[1];
            isPatientClass = complexName == className;
        }

        if (isPatientClass) interfaces.Add($"{Namespace}.IPatient");

        var identifierElement = getIdentifierElement(complex);
        if (identifierElement is not null)
        {
            if (identifierElement.cgIsArray())
                interfaces.Add("IIdentifiable<List<Identifier>>");
            else
                interfaces.Add("IIdentifiable<Identifier>");
        }

        var primaryCodeElementInfo = isResource ? getPrimaryCodedElementInfo(complex, exportName) : null;

        if (primaryCodeElementInfo is not null)
            interfaces.Add($"ICoded<{primaryCodeElementInfo.PropertyType}>");

        var modifierElement = complex.cgGetChild("modifierExtension");
        if (modifierElement is not null)
        {
            if (!modifierElement.cgIsInherited(complex.Structure))
            {
                interfaces.Add($"{Namespace}.IModifierExtendable");
            }
        }

        string interfacesSuffix = interfaces.Any() ? $", {string.Join(", ", interfaces)}" : string.Empty;

        _writer.WriteLineIndented(
            $"public{abstractFlag} partial class" +
                $" {exportName}" +
                $" : {Namespace}.{DetermineExportedBaseTypeName(complex.cgBaseTypeName(_info))}{interfacesSuffix}");

        // open class
        OpenScope();

        WritePropertyTypeName(complex.cgName());

        string validationRegEx = complex.cgValidationRegEx();
        if (!string.IsNullOrEmpty(validationRegEx))
        {
            WriteIndentedComment(
                $"Must conform to pattern \"{validationRegEx}\"",
                false);

            _writer.WriteLineIndented($"public const string PATTERN = @\"{validationRegEx}\";");

            _writer.WriteLine(string.Empty);
        }

        WriteEnums(complex, exportName);

        // check for nested components
        foreach (ComponentDefinition component in complex.cgChildComponents(_info))
        {
            string componentExportName;

            if (string.IsNullOrEmpty(component.cgExplicitName()))
            {
                componentExportName =
                    $"{component.cgName(NamingConvention.PascalCase)}Component";
            }
            else
            {
                // Consent.provisionActorComponent is explicit lower case...
                componentExportName =
                    $"{component.cgExplicitName()}" +
                    $"Component";
            }

            WriteBackboneComponent(
                component,
                componentExportName,
                exportName,
                subset);
        }

        WriteElements(complex, exportName, ref exportedElements, subset);

        if (identifierElement is not null)
        {
            if (identifierElement.cgIsArray())
                _writer.WriteLineIndented("List<Identifier> IIdentifiable<List<Identifier>>.Identifier { get => Identifier; set => Identifier = value; }");
            else
                _writer.WriteLineIndented("Identifier IIdentifiable<Identifier>.Identifier { get => Identifier; set => Identifier = value; }");

            _writer.WriteLine(string.Empty);
        }

        if (primaryCodeElementInfo is not null)
        {
            _writer.WriteLineIndented($"{primaryCodeElementInfo.PropertyType} ICoded<{primaryCodeElementInfo.PropertyType}>.Code {{ get => {primaryCodeElementInfo.PropertyName}; set => {primaryCodeElementInfo.PropertyName} = value; }}");
            _writer.WriteLineIndented($"IEnumerable<Coding> ICoded.ToCodings() => {primaryCodeElementInfo.PropertyName}.ToCodings();");
            _writer.WriteLine(string.Empty);
        }

        if (isPatientClass)
        {
            var birthdayProperty = exportedElements.SingleOrDefault(ee => ee.FhirElementName + ".value" == _cqlModelInfo?.patientBirthDatePropertyName);

            if (birthdayProperty is not null)
            {
                _writer.WriteLineIndented($"Hl7.Fhir.Model.Date {Namespace}.IPatient.BirthDate => {birthdayProperty.PropertyName};");
                _writer.WriteLine(string.Empty);
            }
        }

        WriteCopyTo(exportName, exportedElements);

        if (!isAbstract)
        {
            WriteDeepCopy(exportName);
        }

        WriteMatches(exportName, exportedElements);
        WriteIsExactly(exportName, exportedElements);
        WriteChildren(exportedElements);
        WriteNamedChildren(exportedElements);

        WriteIDictionarySupport(exportedElements);

        // close class
        CloseScope();

        WrittenElementInfo? getPrimaryCodedElementInfo(ComponentDefinition complex, string exportName)
        {
            var primaryCodePath = _cqlModelClassInfo?.TryGetValue(complex.cgName(), out var classInfo) == true && !string.IsNullOrEmpty(classInfo.primaryCodePath)
                ? (complex.cgName() + "." + classInfo.primaryCodePath)
                : null;

            var elem = primaryCodePath is not null ? (tryFindElementInComplex(complex, primaryCodePath, out var e) ? e : null) : null;
            var primaryCodeElementInfo = elem is not null ? BuildElementInfo(exportName, elem) : null;

            if (primaryCodePath is not null && primaryCodeElementInfo is null)
            {
                Console.WriteLine($"Warning: Cannot locate primary code path {primaryCodePath}, so no ICoded<T> was added to this type's signature.");
            }

            return primaryCodeElementInfo;
        }
    }

    private bool tryFindElementInComplex(ComponentDefinition cplx, string name, out ElementDefinition elem)
    {
        if (cplx.Structure.cgTryGetElementByPath(name, out elem!)) return true;
        if (cplx.Structure.cgTryGetElementByPath(name + "[x]", out elem!)) return true;

        return false;
    }

    private string DetermineExportedBaseTypeName(string baseTypeName)
    {
        // These two classes are more like interfaces, we treat their subclasses
        // as subclasses of DomainResource instead.
        if (baseTypeName == "MetadataResource" || baseTypeName == "CanonicalResource")
        {
            return "DomainResource";
        }

        if (_info.FhirSequence < FhirReleases.FhirSequenceCodes.R5)
        {
            // Promote R4 datatypes (all derived from Element/BackboneElement) to the right new subclass
            if (baseTypeName == "BackboneElement" && _info.FhirSequence > FhirReleases.FhirSequenceCodes.STU3)
            {
                return "BackboneType";
            }

            if (baseTypeName == "Element")
            {
                return "DataType";
            }
        }

        return baseTypeName;
    }

    private void WriteIDictionarySupport(IEnumerable<WrittenElementInfo> exportedElements)
    {
        WriteDictionaryTryGetValue(exportedElements);
        WriteDictionaryPairs(exportedElements);
    }


    private string NullCheck(WrittenElementInfo info) => info.PropertyName + (!info.IsList ? " is not null" : "?.Any() == true");

    private void WriteDictionaryPairs(IEnumerable<WrittenElementInfo> exportedElements)
    {
        if (!exportedElements.Any())
        {
            return;
        }

        _writer.WriteLineIndented("protected override IEnumerable<KeyValuePair<string, object>> GetElementPairs()");
        OpenScope();
        _writer.WriteLineIndented("foreach (var kvp in base.GetElementPairs()) yield return kvp;");

        foreach (WrittenElementInfo info in exportedElements)
        {
            string elementProp = $"\"{info.FhirElementName}\"";
            _writer.WriteLineIndented($"if ({NullCheck(info)}) yield return new " +
                $"KeyValuePair<string,object>({elementProp},{info.PropertyName});");
        }

        CloseScope();
    }

    private void WriteDictionaryTryGetValue(IEnumerable<WrittenElementInfo> exportedElements)
    {
        // Don't override anything if there are no additional elements.
        if (!exportedElements.Any())
        {
            return;
        }

        _writer.WriteLineIndented("protected override bool TryGetValue(string key, out object value)");
        OpenScope();

        // switch
        _writer.WriteLineIndented("switch (key)");
        OpenScope();

        foreach (WrittenElementInfo info in exportedElements)
        {
            _writer.WriteLineIndented($"case \"{info.FhirElementName}\":");
            _writer.IncreaseIndent();

            _writer.WriteLineIndented($"value = {info.PropertyName};");
            _writer.WriteLineIndented($"return {NullCheck(info)};");

            _writer.DecreaseIndent();

            //hasChoices |= info.IsChoice;
        }

        _writer.WriteLineIndented("default:");
        _writer.IncreaseIndent();
        writeBaseTryGetValue();

        _writer.DecreaseIndent();

        // end switch
        CloseScope(includeSemicolon: false);

        CloseScope();

        void writeBaseTryGetValue() => _writer.WriteLineIndented("return base.TryGetValue(key, out value);");
    }

    /// <summary>Writes the children of this item.</summary>
    /// <param name="exportedElements">The exported elements.</param>
    private void WriteNamedChildren(
        List<WrittenElementInfo> exportedElements)
    {
        _writer.WriteLineIndented("[IgnoreDataMember]");
        _writer.WriteLineIndented("public override IEnumerable<ElementValue> NamedChildren");
        OpenScope();
        _writer.WriteLineIndented("get");
        OpenScope();
        _writer.WriteLineIndented($"foreach (var item in base.NamedChildren) yield return item;");

        foreach (WrittenElementInfo info in exportedElements)
        {
            if (info.IsList)
            {
                _writer.WriteLineIndented(
                    $"foreach (var elem in {info.PropertyName})" +
                        $" {{ if (elem != null)" +
                        $" yield return new ElementValue(\"{info.FhirElementName}\", elem);" +
                        $" }}");
            }
            else
            {
                _writer.WriteLineIndented(
                    $"if ({info.PropertyName} != null)" +
                        $" yield return new ElementValue(\"{info.FhirElementName}\", {info.PropertyName});");
            }
        }

        CloseScope(suppressNewline: true);
        CloseScope();
    }

    /// <summary>Writes the children of this item.</summary>
    /// <param name="exportedElements">The exported elements.</param>
    private void WriteChildren(
        List<WrittenElementInfo> exportedElements)
    {
        _writer.WriteLineIndented("[IgnoreDataMember]");
        _writer.WriteLineIndented("public override IEnumerable<Base> Children");
        OpenScope();
        _writer.WriteLineIndented("get");
        OpenScope();
        _writer.WriteLineIndented($"foreach (var item in base.Children) yield return item;");

        foreach (WrittenElementInfo info in exportedElements)
        {
            if (info.IsList)
            {
                _writer.WriteLineIndented(
                    $"foreach (var elem in {info.PropertyName})" +
                        $" {{ if (elem != null) yield return elem; }}");
            }
            else
            {
                _writer.WriteLineIndented(
                    $"if ({info.PropertyName} != null)" +
                        $" yield return {info.PropertyName};");
            }
        }

        CloseScope(suppressNewline: true);
        CloseScope();
    }

    /// <summary>Writes the matches.</summary>
    /// <param name="exportName">      Name of the export.</param>
    /// <param name="exportedElements">The exported elements.</param>
    private void WriteMatches(
        string exportName,
        List<WrittenElementInfo> exportedElements)
    {
        _writer.WriteLineIndented("///<inheritdoc />");
        _writer.WriteLineIndented("public override bool Matches(IDeepComparable other)");
        OpenScope();
        _writer.WriteLineIndented($"var otherT = other as {exportName};");
        _writer.WriteLineIndented("if(otherT == null) return false;");
        _writer.WriteLine(string.Empty);
        _writer.WriteLineIndented("if(!base.Matches(otherT)) return false;");

        foreach (WrittenElementInfo info in exportedElements)
        {
            _writer.WriteLineIndented(
                $"if( !DeepComparable.Matches({info.PropertyName}, otherT.{info.PropertyName}))" +
                    $" return false;");
        }

        _writer.WriteLine(string.Empty);
        _writer.WriteLineIndented("return true;");

        CloseScope();
    }

    /// <summary>Writes the is exactly.</summary>
    /// <param name="exportName">      Name of the export.</param>
    /// <param name="exportedElements">The exported elements.</param>
    private void WriteIsExactly(
        string exportName,
        List<WrittenElementInfo> exportedElements)
    {
        _writer.WriteLineIndented("public override bool IsExactly(IDeepComparable other)");
        OpenScope();
        _writer.WriteLineIndented($"var otherT = other as {exportName};");
        _writer.WriteLineIndented("if(otherT == null) return false;");
        _writer.WriteLine(string.Empty);
        _writer.WriteLineIndented("if(!base.IsExactly(otherT)) return false;");

        foreach (WrittenElementInfo info in exportedElements)
        {
            _writer.WriteLineIndented(
                $"if( !DeepComparable.IsExactly({info.PropertyName}, otherT.{info.PropertyName}))" +
                    $" return false;");
        }

        _writer.WriteLine(string.Empty);
        _writer.WriteLineIndented("return true;");

        CloseScope();
    }

    /// <summary>Writes a copy to.</summary>
    /// <exception cref="ArgumentException">Thrown when one or more arguments have unsupported or
    ///  illegal values.</exception>
    /// <param name="exportName">      Name of the export.</param>
    /// <param name="exportedElements">The exported elements.</param>
    private void WriteCopyTo(
        string exportName,
        List<WrittenElementInfo> exportedElements)
    {
        _writer.WriteLineIndented("public override IDeepCopyable CopyTo(IDeepCopyable other)");
        OpenScope();
        _writer.WriteLineIndented($"var dest = other as {exportName};");
        _writer.WriteLine(string.Empty);

        _writer.WriteLineIndented("if (dest == null)");
        OpenScope();
        _writer.WriteLineIndented("throw new ArgumentException(\"Can only copy to an object of the same type\", \"other\");");
        CloseScope();

        _writer.WriteLineIndented("base.CopyTo(dest);");

        foreach (WrittenElementInfo info in exportedElements)
        {
            if (info.IsList)
            {
                _writer.WriteLineIndented(
                    $"if({info.PropertyName} != null)" +
                        $" dest.{info.PropertyName} = new {info.PropertyType}({info.PropertyName}.DeepCopy());");
            }
            else
            {
                _writer.WriteLineIndented(
                    $"if({info.PropertyName} != null)" +
                        $" dest.{info.PropertyName} = ({info.PropertyType}){info.PropertyName}.DeepCopy();");
            }
        }

        _writer.WriteLineIndented("return dest;");

        CloseScope();
    }

    /// <summary>Writes a deep copy.</summary>
    /// <param name="exportName">Name of the export.</param>
    private void WriteDeepCopy(
        string exportName)
    {
        _writer.WriteLineIndented("public override IDeepCopyable DeepCopy()");
        OpenScope();
        _writer.WriteLineIndented($"return CopyTo(new {exportName}());");
        CloseScope();
    }

    /// <summary>Writes a constrained quantity.</summary>
    /// <param name="complex">   The complex data type.</param>
    /// <param name="exportName">Name of the export.</param>
    private void WriteConstrainedQuantity(
        ComponentDefinition complex,
        string exportName)
    {
        _writer.WriteLineIndented(
            $"public partial class" +
                $" {exportName}" +
                $" : Quantity");

        // open class
        OpenScope();

        WritePropertyTypeName(complex.Structure.Name);

        _writer.WriteLineIndented("public override IDeepCopyable DeepCopy()");
        OpenScope();
        _writer.WriteLineIndented($"return CopyTo(new {exportName}());");
        CloseScope();

        _writer.WriteLineIndented("// TODO: Add code to enforce these constraints:");
        WriteIndentedComment(complex.Structure.Purpose, isSummary: false, singleLine: true);

        // close class
        CloseScope();
    }

    private string capitalizeThoseSillyBackboneNames(string path) =>
        path.Length == 1 ? path :
               path.StartsWith(".") ?
                char.ToUpper(path[1]) + capitalizeThoseSillyBackboneNames(path.Substring(2))
                : path[0] + capitalizeThoseSillyBackboneNames(path.Substring(1));

    /// <summary>Writes a component.</summary>
    /// <param name="complex">              The complex data type.</param>
    /// <param name="exportName">           Name of the export.</param>
    /// <param name="parentExportName">     Name of the parent export.</param>
    /// <param name="subset"></param>
    private void WriteBackboneComponent(
        ComponentDefinition complex,
        string exportName,
        string parentExportName,
        GenSubset subset)
    {
        List<WrittenElementInfo> exportedElements = new List<WrittenElementInfo>();

        WriteIndentedComment($"{complex.Element.Short}");

        string explicitName = complex.cgExplicitName();

        // ginoc 2024.03.12: Release has happened and these are no longer needed - leaving here but commented out until confirmed
        /*
        // TODO: the following renames (repairs) should be removed when release 4B is official and there is an
        //   explicitname in the definition for attributes:
        //   - Statistic.attributeEstimate.attributeEstimate
        //   - Citation.contributorship.summary

        if (complex.Id.StartsWith("Citation") || complex.Id.StartsWith("Statistic") || complex.Id.StartsWith("DeviceDefinition"))
        {
            string parentName = complex.Id.Substring(0, complex.Id.IndexOf('.'));
            var sillyBackboneName = complex.Id.Substring(parentName.Length);
            explicitName = capitalizeThoseSillyBackboneNames(sillyBackboneName);
            exportName = explicitName + "Component";
        }
        // end of repair
        */

        string explicitNamePart = string.IsNullOrEmpty(explicitName) ?
            complex.cgName(NamingConvention.PascalCase) :
            explicitName;
        string componentName = parentExportName + "#" + explicitNamePart;

        WriteSerializable();
        _writer.WriteLineIndented($"[FhirType(\"{componentName}\", IsNestedType=true)]");

        _writer.WriteLineIndented($"[BackboneType(\"{complex.Element.Path}\")]");

        _writer.WriteLineIndented(
            $"public partial class" +
                $" {exportName}" +
                $" : {Namespace}.{complex.cgBaseTypeName(_info)}");

        // open class
        OpenScope();

        WritePropertyTypeName(componentName);

        WriteElements(complex, exportName, ref exportedElements, subset);

        if (exportedElements.Count > 0)
        {
            WriteCopyTo(exportName, exportedElements);
        }

        WriteDeepCopy(exportName);

        if (exportedElements.Count > 0)
        {
            WriteMatches(exportName, exportedElements);
            WriteIsExactly(exportName, exportedElements);
            WriteChildren(exportedElements);
            WriteNamedChildren(exportedElements);
            WriteIDictionarySupport(exportedElements);
        }

        // close class
        CloseScope();

        // check for nested components
        foreach (ComponentDefinition component in complex.cgChildComponents(_info))
        {
            string componentExportName;

            if (string.IsNullOrEmpty(component.cgExplicitName()))
            {
                componentExportName =
                    $"{component.cgName(NamingConvention.PascalCase)}Component";
            }
            else
            {
                // Consent.provisionActorComponent is explicit lower case...
                componentExportName =
                    $"{component.cgExplicitName()}" +
                    $"Component";
            }

            WriteBackboneComponent(
                component,
                componentExportName,
                exportName,
                subset);
        }
    }

    /// <summary>Writes the enums.</summary>
    /// <param name="complex">      The complex data type.</param>
    /// <param name="className">    Name of the class this enum is being written in.</param>
    /// <param name="usedEnumNames">(Optional) List of names of the used enums.</param>
    private void WriteEnums(
        ComponentDefinition complex,
        string className,
        HashSet<string>? usedEnumNames = null)
    {
        if (usedEnumNames == null)
        {
            usedEnumNames = new HashSet<string>();
        }

        IEnumerable<ElementDefinition> childElements = complex.cgGetChildren();

        if (childElements.Any())
        {
            foreach (ElementDefinition element in childElements)
            {
                if ((!string.IsNullOrEmpty(element.Binding?.ValueSet)) &&
                    (element.Binding.Strength == Hl7.Fhir.Model.BindingStrength.Required) &&
                    _info.TryExpandVs(element.Binding.ValueSet, out ValueSet? vs))
                {
                    WriteEnum(vs, className, usedEnumNames);
                }
            }
        }

        foreach (ComponentDefinition component in complex.cgChildComponents(_info))
        {
            WriteEnums(component, className, usedEnumNames);
        }
    }

    /// <summary>Writes a value set as an enum.</summary>
    /// <param name="vs">       The vs.</param>
    /// <param name="className">Name of the class this enum is being written in.</param>
    /// <param name="usedEnumNames"></param>
    /// <param name="silent">Do not actually write parameter to file, just add it in memory.</param>
    private void WriteEnum(
        ValueSet vs,
        string className,
        HashSet<string> usedEnumNames,
        bool silent = false)
    {
        if (_writtenValueSets.ContainsKey(vs.Url))
        {
            return;
        }

        if (_exclusionSet.Contains(vs.Url))
        {
            return;
        }

        string name = (vs.Name ?? vs.Id)
            .Replace(" ", string.Empty, StringComparison.Ordinal)
            .Replace("_", string.Empty, StringComparison.Ordinal);

        string nameSanitized = FhirSanitizationUtils.SanitizeForProperty(name, _reservedWords, NamingConvention.PascalCase);

        // Enums and their containing classes cannot have the same name,
        // so we have to correct these here
        if (_enumNamesOverride.TryGetValue(vs.Url, out var replacementName))
        {
            nameSanitized = replacementName;
        }

        if (usedEnumNames.Contains(nameSanitized))
        {
            return;
        }

        usedEnumNames.Add(nameSanitized);

        if (silent)
        {
            _writtenValueSets.Add(
                vs.Url,
                new WrittenValueSetInfo()
                {
                    ClassName = className,
                    ValueSetName = nameSanitized,
                });

            return;
        }

        IEnumerable<string> referencedCodeSystems = vs.cgReferencedCodeSystems();

        if (referencedCodeSystems.Count() == 1)
        {
            WriteIndentedComment(
                $"{vs.Description}\n" +
                $"(url: {vs.Url})\n" +
                $"(system: {referencedCodeSystems.First()})");
        }
        else
        {
            WriteIndentedComment(
                $"{vs.Description}\n" +
                $"(url: {vs.Url})\n" +
                $"(systems: {referencedCodeSystems.Count()})");
        }

        IEnumerable<FhirConcept> concepts = vs.cgGetFlatConcepts();

        var defaultSystem = GetDefaultCodeSystem(concepts);

        _writer.WriteLineIndented($"[FhirEnumeration(\"{name}\", \"{vs.Url}\", \"{defaultSystem}\")]");

        _writer.WriteLineIndented($"public enum {nameSanitized}");

        OpenScope();

        HashSet<string> usedLiterals = new HashSet<string>();

        foreach (FhirConcept concept in concepts)
        {
            string codeName = ConvertEnumValue(concept.Code);
            string codeValue = FhirSanitizationUtils.SanitizeForValue(concept.Code);
            string description = string.IsNullOrEmpty(concept.Display)
                ? $"MISSING DESCRIPTION\n(system: {concept.System})"
                : $"{concept.Display}\n(system: {concept.System})";

            if (concept.HasProperty("status", "deprecated"))
            {
                description += "\nThis enum is DEPRECATED.";
            }

            WriteIndentedComment(description);

            string display = FhirSanitizationUtils.SanitizeForValue(concept.Display);

            if (concept.System != defaultSystem)
                _writer.WriteLineIndented($"[EnumLiteral(\"{codeValue}\", \"{concept.System}\"), Description(\"{display}\")]");
            else
                _writer.WriteLineIndented($"[EnumLiteral(\"{codeValue}\"), Description(\"{display}\")]");

            if (usedLiterals.Contains(codeName))
            {
                // start at 2 so that the unadorned version makes sense as v1
                for (int i = 2; i < 1000; i++)
                {
                    if (usedLiterals.Contains($"{codeName}_{i}"))
                    {
                        continue;
                    }

                    codeName = $"{codeName}_{i}";
                    break;
                }
            }

            usedLiterals.Add(codeName);

            _writer.WriteLineIndented($"{codeName},");
        }

        CloseScope();

        _writtenValueSets.Add(
            vs.Url,
            new WrittenValueSetInfo()
            {
                ClassName = className,
                ValueSetName = nameSanitized,
            });
    }

    private static string GetDefaultCodeSystem(IEnumerable<FhirConcept> concepts)
    {
        return concepts.Select(c => c.System)
                        .GroupBy(c => c)
                        .OrderByDescending(c => c.Count())
                        .First().Key;
    }

    /// <summary>Convert enum value - see Template-Model.tt#2061.</summary>
    /// <param name="name">The name.</param>
    /// <returns>The enum converted value.</returns>
    private static string ConvertEnumValue(string name) => CSharpFirelyCommon.ConvertEnumValue(name);

    private static ElementDefinition? getIdentifierElement(ComponentDefinition cd)
    {
        if (!cd.IsRootOfStructure)
        {
            return null;
        }

        if (cd.Structure.Snapshot?.Element.Any() ?? false)
        {
            return cd.Structure.Snapshot.Element.SingleOrDefault(e => isIdentifierProperty(e));
        }

        return cd.Structure.Differential?.Element.SingleOrDefault(e => isIdentifierProperty(e));
    }

    /// <summary>
    /// Determines whether this element qualifies as an identifying element.
    /// </summary>
    /// <param name="element"></param>
    /// <returns></returns>
    private static bool isIdentifierProperty(ElementDefinition element)
    {
        return element.Path.EndsWith(".identifier", StringComparison.Ordinal) &&
            (element.Type.Count == 1) &&
            element.Type.First().Code.Equals("Identifier", StringComparison.Ordinal);
    }

    /// <summary>Writes the elements.</summary>
    /// <param name="complex">              The complex data type.</param>
    /// <param name="exportedComplexName">  Name of the exported complex parent.</param>
    /// <param name="exportedElements">     [in,out] The exported elements.</param>
    /// <param name="subset"></param>
    private void WriteElements(
        ComponentDefinition complex,
        string exportedComplexName,
        ref List<WrittenElementInfo> exportedElements,
        GenSubset subset)
    {
        var elementsToGenerate = complex.cgGetChildren()
            .Where(e => !e.cgIsInherited(complex.Structure))
            .OrderBy(e => e.cgFieldOrder());

        foreach (ElementDefinition element in elementsToGenerate)
        {
            WriteElement(
                exportedComplexName,
                element,
                ref exportedElements,
                subset);
        }
    }

    private WrittenElementInfo BuildCodedElementInfo(ElementDefinition element)
    {
        bool hasDefinedEnum = true;

        if ((element.Binding?.Strength != Hl7.Fhir.Model.BindingStrength.Required) ||
            (!_info.TryExpandVs(element.Binding.ValueSet, out ValueSet? vs)) ||
            _exclusionSet.Contains(vs.Url) ||
            (_codedElementOverrides.Contains(element.Path) && _info.FhirSequence >= FhirReleases.FhirSequenceCodes.R4)
            )
        {
            hasDefinedEnum = false;
            vs = null;
        }

        string codeLiteral;
        string enumClass;
        string optional;
        string rawName = element.cgName();
        string pascal = rawName.ToPascalCase();

        if (hasDefinedEnum)
        {
            string vsClass = _writtenValueSets[vs!.Url].ClassName;
            string vsName = _writtenValueSets[vs.Url].ValueSetName;

            if (string.IsNullOrEmpty(vsClass))
            {
                codeLiteral = $"Code<{Namespace}.{vsName}>";
                enumClass = $"{Namespace}.{vsName}";
            }
            else
            {
                codeLiteral = $"Code<{Namespace}.{vsClass}.{vsName}>";
                enumClass = $"{Namespace}.{vsClass}.{vsName}";

                if (vsName.ToUpperInvariant() == pascal.ToUpperInvariant())
                {
                    throw new InvalidOperationException($"Using the name '{pascal}' for the property would lead to a compiler error. " +
                        $"Change the name of the valueset '{vs.Url}' by adapting the _enumNamesOverride variable in the generator and rerun.");
                }
            }

            optional = "?";
        }
        else
        {
            codeLiteral = $"{Namespace}.Code";
            enumClass = "string";
            optional = string.Empty;
        }


        bool isList = element.cgIsArray();

        var ei = new WrittenElementInfo()
        {
            FhirElementName = rawName.Replace("[x]", string.Empty, StringComparison.Ordinal),
            PropertyName = $"{pascal}Element",
            PropertyType = isList ? $"List<{codeLiteral}>" : codeLiteral,
            ElementType = codeLiteral,
            IsList = isList,
            IsChoice = rawName.Contains("[x]", StringComparison.Ordinal),
            PrimitiveHelperName = pascal,
            PrimitiveHelperType = $"{enumClass}{optional}",
            IsPrimitive = true, // All coded elements are either Code or Code<T> - so primitives
            IsCodedEnum = hasDefinedEnum
        };

        return ei;
    }

    private void BuildFhirElementAttribute(string name, string summary, string? isModifier, ElementDefinition element, string choice, string fiveWs, string? since = null, (string, string)? until = null)
    {
        string attributeText = $"[FhirElement(\"{name}\"{summary}{isModifier}, Order={CSharpFirelyCommon.GetOrder(element)}{choice}{fiveWs}";
        if (since is { })
        {
            attributeText += $", Since=FhirRelease.{since}";
        }

        attributeText += ")]";
        _writer.WriteLineIndented(attributeText);

        if (until is not null)
        {
            _writer.WriteLineIndented($"[NotMapped(Since=FhirRelease.{until.Value.Item1})]");
        }
    }

    /// <summary>Writes an element.</summary>
    /// <param name="exportedComplexName">Name of the exported complex parent.</param>
    /// <param name="element">            The element.</param>
    /// <param name="exportedElements">   [in,out] The exported elements.</param>
    /// <param name="subset"></param>
    private void WriteElement(
        string exportedComplexName,
        ElementDefinition element,
        ref List<WrittenElementInfo> exportedElements,
        GenSubset subset)
    {
        string name = element.cgName().Replace("[x]", string.Empty);

        WrittenElementInfo ei = BuildElementInfo(exportedComplexName, element);
        exportedElements.Add(ei);

        BuildElementOptionals(
            element,
            subset,
            out string summary,
            out string isModifier,
            out string choice,
            out string allowedTypes,
            out string resourceReferences);

        string fiveWs = string.Empty;

        if (_exportFiveWs && (!string.IsNullOrEmpty(element.cgFiveWs())))
        {
            fiveWs = $", FiveWs=\"{element.cgFiveWs()}\"";
        }

        var since = _sinceAttributes.TryGetValue(element.Path, out string? s) ? s : null;
        var until = _untilAttributes.TryGetValue(element.Path, out (string, string) u) ? u : default((string, string)?);

        var description = element.Path switch
        {
            "Signature.who" => element.Short + ".\nNote 1: Since R4 the type of this element should be a fixed type (ResourceReference). For backwards compatibility it remains of type DataType.\nNote 2: Since R5 the cardinality is expanded to 0..1 (previous it was 1..1).",
            "Signature.onBehalfOf" => element.Short + ".\nNote: Since R4 the type of this element should be a fixed type (ResourceReference). For backwards compatibility it remains of type DataType.",
            "Signature.when" => element.Short + ".\nNote: Since R5 the cardinality is expanded to 0..1 (previous it was 1..1).",
            "Signature.type" => element.Short + ".\nNote: Since R5 the cardinality is expanded to 0..* (previous it was 1..*).",
            _ => attributeDescriptionWithSinceInfo(name, element.Short, since, until)
        };

        if (description is not null) WriteIndentedComment(description);

        if (element.Path == "OperationOutcome.issue.severity")
        {
            BuildFhirElementAttribute(name, summary, ", IsModifier=true", element, choice, fiveWs);
            BuildFhirElementAttribute(name, summary, null, element, choice, fiveWs, since: "R4");
        }
        else if (element.Path is "Signature.who" or "Signature.onBehalfOf")
        {
            BuildFhirElementAttribute(name, summary, isModifier, element, ", Choice = ChoiceType.DatatypeChoice", fiveWs);
            BuildFhirElementAttribute(name, summary, isModifier, element, "", fiveWs, since: since);
            _writer.WriteLineIndented($"[DeclaredType(Type = typeof(ResourceReference), Since = FhirRelease.R4)]");
            _writer.WriteLineIndented($"[AllowedTypes(typeof(Hl7.Fhir.Model.FhirUri), typeof(Hl7.Fhir.Model.ResourceReference))]");
        }
        else
        {
            BuildFhirElementAttribute(name, summary, isModifier, element, choice, fiveWs, since, until);
        }

        if (element.Path == "Meta.profile")
        {
            _writer.WriteLineIndented($"[DeclaredType(Type = typeof(Canonical), Since = FhirRelease.R4)]");
        }
        else if (element.Path == "Bundle.link.relation")
        {
            _writer.WriteLineIndented($"[DeclaredType(Type = typeof(Code), Since = FhirRelease.R5)]");
        }
        else if (element.Path == "Attachment.size")
        {
            _writer.WriteLineIndented($"[DeclaredType(Type = typeof(UnsignedInt), Since = FhirRelease.STU3)]");
            _writer.WriteLineIndented($"[DeclaredType(Type = typeof(Integer64), Since = FhirRelease.R5)]");
        }
        else if (element.Path is
            "ElementDefinition.constraint.requirements" or
            "ElementDefinition.binding.description" or
            "ElementDefinition.mapping.comment" or
            "CapabilityStatement.implementation.description")
        {
            _writer.WriteLineIndented($"[DeclaredType(Type = typeof(FhirString))]");
            _writer.WriteLineIndented($"[DeclaredType(Type = typeof(Markdown), Since = FhirRelease.R5)]");
        }

        if (ei.IsCodedEnum)
        {
            _writer.WriteLineIndented("[DeclaredType(Type = typeof(Code))]");
        }

        if (!string.IsNullOrEmpty(element.cgBindingName()))
        {
            _writer.WriteLineIndented($"[Binding(\"{element.cgBindingName()}\")]");
        }

        // Generate the [AllowedTypes] and [ResourceReference] attributes, except when we are
        // generating datatypes and resources in the base subset, since this list probably contains
        // classes that we have not yet moved to that subset.
        bool notClsCompliant = !string.IsNullOrEmpty(allowedTypes) ||
            !string.IsNullOrEmpty(resourceReferences);

        if (notClsCompliant)
        {
            _writer.WriteLineIndented("[CLSCompliant(false)]");
        }

        if (!string.IsNullOrEmpty(resourceReferences))
        {
            if (element.Path is "Signature.who" or "Signature.onBehalfOf")
            {
                _writer.WriteLineIndented($"[References(\"Practitioner\",\"RelatedPerson\",\"Patient\",\"Device\",\"Organization\")]");
                _writer.WriteLineIndented($"[References(\"Practitioner\",\"PractitionerRole\",\"RelatedPerson\",\"Patient\",\"Device\",\"Organization\", Since=FhirRelease.R4)]");

            }
            else
            {
                _writer.WriteLineIndented(resourceReferences);
            }
        }

        if (!string.IsNullOrEmpty(allowedTypes))
        {
            _writer.WriteLineIndented(allowedTypes);
        }

        if ((element.Min != 0) ||
            (element.cgCardinalityMax() != 1))
        {
            _writer.WriteLineIndented($"[Cardinality(Min={element.Min},Max={element.Max})]");
        }

        writeElementGettersAndSetters(element, ei);
    }

    private string? attributeDescriptionWithSinceInfo(string name, string baseDescription, string? since = null, (string, string)? until = null)
    {
        return (since, until, baseDescription) switch
        {
            (_, _, null) => null,
            (not null, _, _) => baseDescription +
                             $". Note: Element was introduced in {since}, do not use when working with older releases.",
            (_, (var release, ""), _) when until is not null => baseDescription +
                             $". Note: Element is deprecated since {release}, do not use with {release} and newer releases.",
            (_, (var release, var replacedBy), _) when until is not null => baseDescription +
                             $". Note: Element is replaced by '{replacedBy}' since {release}. Do not use this element '{name}' with {release} and newer releases.",
            _ => baseDescription
        };
    }

    private WrittenElementInfo BuildElementInfo(string exportedComplexName, ElementDefinition element)
    {
        string type;
        var name = element.cgName().Replace("[x]", string.Empty);

        if (!string.IsNullOrEmpty(element.cgBaseTypeName(_info)))
        {
            type = element.cgBaseTypeName(_info);
        }
        else if (element.Type.Count == 1)
        {
            type = element.Type.First().Code;
        }
        else
        {
            type = "DataType";
        }

        // Elements of type Code or Code<T> have their own naming/types, so handle those separately.
        if (type == "code")
            return BuildCodedElementInfo(element);

        /* This is an exception - we want to share Meta across different FHIR versions,
         * so we use the "most common" type to the versions, which
         * is uri rather than the more specific canonical. */
        if (element.Path == "Meta.profile")
        {
            type = "uri";
        }

        bool isPrimitive = false;

        if (type.Contains('.'))
        {
            type = BuildTypeFromPath(element, type);
        }

        string nativeType = type;
        string optional = string.Empty;

        if (CSharpFirelyCommon.PrimitiveTypeMap.ContainsKey(nativeType))
        {
            nativeType = CSharpFirelyCommon.PrimitiveTypeMap[nativeType];

            if (IsNullable(nativeType))
            {
                optional = "?";
            }

            isPrimitive = true;
        }
        else
        {
            nativeType = $"{Namespace}.{type}";
        }

        if (CSharpFirelyCommon.TypeNameMappings.ContainsKey(type))
        {
            type = CSharpFirelyCommon.TypeNameMappings[type];
        }
        else
        {
            type = type.ToPascalCase();
        }

        WrittenElementInfo ei;
        bool isList = element.cgIsArray();
        string pascal = name.ToPascalCase();

        ei = new WrittenElementInfo()
        {
            FhirElementName = name,
            PropertyName = isPrimitive ? $"{pascal}Element" : pascal,
            PropertyType = isList ? $"List<{Namespace}.{type}>" : $"{Namespace}.{type}",
            ElementType = $"{Namespace}.{type}",
            IsList = isList,
            IsChoice = element.cgName().Contains("[x]", StringComparison.Ordinal),
            IsPrimitive = isPrimitive,
            PrimitiveHelperName = isPrimitive ? (pascal == exportedComplexName ? $"{pascal}_" : pascal) : null,              // Since properties cannot have the same name as their enclosing types, we'll add a '_' suffix if this happens.
            PrimitiveHelperType = isPrimitive ? nativeType + optional : null
        };
        return ei;
    }

    private void writeElementGettersAndSetters(ElementDefinition element, WrittenElementInfo ei)
    {
        _writer.WriteLineIndented("[DataMember]");

        if (!ei.IsList)
        {
            _writer.WriteLineIndented($"public {ei.PropertyType} {ei.PropertyName}");

            OpenScope();
            _writer.WriteLineIndented($"get {{ return _{ei.PropertyName}; }}");
            _writer.WriteLineIndented($"set {{ _{ei.PropertyName} = value; OnPropertyChanged(\"{ei.PropertyName}\"); }}");
            CloseScope();

            _writer.WriteLineIndented($"private {ei.PropertyType} _{ei.PropertyName};");
            _writer.WriteLine(string.Empty);
        }
        else
        {
            _writer.WriteLineIndented($"public {ei.PropertyType} {ei.PropertyName}");

            OpenScope();
            _writer.WriteLineIndented($"get {{ if(_{ei.PropertyName}==null) _{ei.PropertyName} = new {ei.PropertyType}(); return _{ei.PropertyName}; }}");
            _writer.WriteLineIndented($"set {{ _{ei.PropertyName} = value; OnPropertyChanged(\"{ei.PropertyName}\"); }}");
            CloseScope();

            _writer.WriteLineIndented($"private {ei.PropertyType} _{ei.PropertyName};");
            _writer.WriteLine(string.Empty);
        }

        if (ei.IsPrimitive)
        {
            WriteIndentedComment(element.Short);
            _writer.WriteLineIndented($"/// <remarks>This uses the native .NET datatype, rather than the FHIR equivalent</remarks>");

            _writer.WriteLineIndented("[IgnoreDataMember]");

            if (!ei.IsList)
            {
                _writer.WriteLineIndented($"public {ei.PrimitiveHelperType} {ei.PrimitiveHelperName}");

                OpenScope();
                _writer.WriteLineIndented($"get {{ return {ei.PropertyName} != null ? {ei.PropertyName}.Value : null; }}");
                _writer.WriteLineIndented("set");
                OpenScope();

                _writer.WriteLineIndented($"if (value == null)");

                _writer.IncreaseIndent();
                _writer.WriteLineIndented($"{ei.PropertyName} = null;");
                _writer.DecreaseIndent();
                _writer.WriteLineIndented("else");
                _writer.IncreaseIndent();
                _writer.WriteLineIndented($"{ei.PropertyName} = new {ei.PropertyType}(value);");
                _writer.DecreaseIndent();
                _writer.WriteLineIndented($"OnPropertyChanged(\"{ei.PrimitiveHelperName}\");");
                CloseScope(suppressNewline: true);
                CloseScope();
            }
            else
            {
                _writer.WriteLineIndented($"public IEnumerable<{ei.PrimitiveHelperType}> {ei.PrimitiveHelperName}");

                OpenScope();
                _writer.WriteLineIndented($"get {{ return {ei.PropertyName} != null ? {ei.PropertyName}.Select(elem => elem.Value) : null; }}");
                _writer.WriteLineIndented("set");
                OpenScope();

                _writer.WriteLineIndented($"if (value == null)");

                _writer.IncreaseIndent();
                _writer.WriteLineIndented($"{ei.PropertyName} = null;");
                _writer.DecreaseIndent();
                _writer.WriteLineIndented("else");
                _writer.IncreaseIndent();
                _writer.WriteLineIndented($"{ei.PropertyName} = new {ei.PropertyType}(value.Select(elem=>new {ei.ElementType}(elem)));");
                _writer.DecreaseIndent();

                _writer.WriteLineIndented($"OnPropertyChanged(\"{ei.PrimitiveHelperName}\");");
                CloseScope(suppressNewline: true);
                CloseScope();
            }
        }
    }

    /// <summary>Builds type from path.</summary>
    /// <param name="type">The type.</param>
    /// <returns>A string.</returns>
    private string BuildTypeFromPath(ElementDefinition ed, string type)
    {
        // ginoc 2024.03.12: Release has happened and these are no longer needed - leaving here but commented out until confirmed
        /*
        // TODO: the following renames (repairs) should be removed when release 4B is official and there is an
        //   explicitname in the definition for attributes:
        //   - Statistic.attributeEstimate.attributeEstimate
        //   - Citation.contributorship.summary

        if (type.StartsWith("Citation") || type.StartsWith("Statistic") || type.StartsWith("DeviceDefinition"))
        {
            string parentName = type.Substring(0, type.IndexOf('.'));
            var sillyBackboneName = type.Substring(parentName.Length);
            type = parentName + "." + capitalizeThoseSillyBackboneNames(sillyBackboneName) + "Component";
        }
        // end of repair
        */

        string explicitTypeName = ed.cgExplicitName();

        if (!string.IsNullOrEmpty(explicitTypeName))
        {
            string parentName = type.Substring(0, type.IndexOf('.'));
            type = $"{parentName}" +
                $".{explicitTypeName}" +
                $"Component";
        }
        else
        {
            StringBuilder sb = new StringBuilder();

            string[] components = type.Split('.');

            for (int i = 0; i < components.Length; i++)
            {
                if (i == 0)
                {
                    sb.Append(components[i]);
                    continue;
                }

                if (i == 1)
                {
                    sb.Append(".");
                }

                // AdverseEvent.suspectEntity.Causality does not prefix?
                if (i == components.Length - 1)
                {
                    sb.Append(FhirSanitizationUtils.SanitizedToConvention(components[i], NamingConvention.PascalCase));
                }
            }

            sb.Append("Component");
            type = sb.ToString();
        }

        return type;
    }

    /// <summary>Builds element optional flags.</summary>
    /// <param name="element">           The element.</param>
    /// <param name="subset"></param>
    /// <param name="summary">           [out] The summary.</param>
    /// <param name="isModifier"></param>
    /// <param name="choice">            [out] The choice.</param>
    /// <param name="allowedTypes">      [out] List of types of the allowed.</param>
    /// <param name="resourceReferences">[out] The resource references.</param>
    private void BuildElementOptionals(
        ElementDefinition element,
        GenSubset subset,
        out string summary,
        out string isModifier,
        out string choice,
        out string allowedTypes,
        out string resourceReferences)
    {
        choice = string.Empty;
        allowedTypes = string.Empty;
        resourceReferences = string.Empty;
        summary = element.IsSummary == true ? ", InSummary=true" : string.Empty;
        isModifier = element.IsModifier == true ? ", IsModifier=true" : string.Empty;

        IReadOnlyDictionary<string, ElementDefinition.TypeRefComponent> elementTypes = element.cgTypes();

        if (elementTypes.Any())
        {
            if (elementTypes.Count == 1)
            {
                string elementType = elementTypes.First().Key;

                if (elementType == "Resource")
                {
                    choice = ", Choice=ChoiceType.ResourceChoice";
                    allowedTypes = $"[AllowedTypes(typeof({Namespace}.Resource))]";
                }
            }
            else
            {
                string firstType = elementTypes.First().Key;

                if (_info.PrimitiveTypesByName.ContainsKey(firstType) ||
                    _info.ComplexTypesByName.ContainsKey(firstType))
                {
                    choice = ", Choice=ChoiceType.DatatypeChoice";
                }

                if (_info.ResourcesByName.ContainsKey(firstType))
                {
                    choice = ", Choice=ChoiceType.ResourceChoice";
                }

                // When we generating classes in the base subset, we have to avoid generating an
                // [AllowedTypes] attribute that contains class names that are not
                // present in the current version of the standard. So, in principle, we don't generate
                // this attribute in the base subset, unless all types mentioned are present in the
                // exception list above.
                bool isPrimive(string name) => char.IsLower(name[0]);
                bool allTypesAvailable =
                    elementTypes.Keys.All(en =>
                        isPrimive(en) // primitives are available everywhere
                        || _baseSubsetComplexTypes.Contains(en) // base subset types are all available everywhere
                        || (subset.HasFlag(GenSubset.Conformance) && _conformanceSubsetComplexTypes.Contains(en)) // otherwise, conformance types are available in conformance
                        || subset.HasFlag(GenSubset.Satellite)  // main has access to all types
                        );

                if (allTypesAvailable)
                {
                    StringBuilder sb = new StringBuilder();
                    sb.Append("[AllowedTypes(");

                    bool needsSep = false;
                    foreach ((string etName, ElementDefinition.TypeRefComponent elementType) in elementTypes)
                    {
                        if (needsSep)
                        {
                            sb.Append(",");
                        }

                        needsSep = true;

                        sb.Append("typeof(");
                        sb.Append(Namespace);
                        sb.Append(".");

                        if (CSharpFirelyCommon.TypeNameMappings.ContainsKey(etName))
                        {
                            sb.Append(CSharpFirelyCommon.TypeNameMappings[etName]);
                        }
                        else
                        {
                            sb.Append(FhirSanitizationUtils.SanitizedToConvention(etName, NamingConvention.PascalCase));
                        }

                        sb.Append(")");
                    }

                    sb.Append(")]");
                    allowedTypes = sb.ToString();
                }
            }
        }

        if (elementTypes.Any())
        {
            foreach ((string etName, ElementDefinition.TypeRefComponent elementType) in elementTypes.Where(kvp => kvp.Key.Equals("Reference") && kvp.Value.TargetProfile.Any()))
            {
                resourceReferences = "[References(" +
                    string.Join(',', elementType.cgTargetProfiles().Keys.Select(name => "\"" + name + "\"")) +
                    ")]";
                break;
            }
        }
    }

    /// <summary>Writes a property type name.</summary>
    /// <param name="name">      The name.</param>
    private void WritePropertyTypeName(string name)
    {
        WriteIndentedComment("FHIR Type Name");

        _writer.WriteLineIndented($"public override string TypeName {{ get {{ return \"{name}\"; }} }}");

        _writer.WriteLine(string.Empty);
    }

    /// <summary>Query if 'typeName' is nullable.</summary>
    /// <param name="typeName">Name of the type.</param>
    /// <returns>True if nullable, false if not.</returns>
    private static bool IsNullable(string typeName)
    {
        // nullable reference types are not allowed in current C#
        switch (typeName)
        {
            case "bool":
            case "decimal":
            case "DateTime":
            case "int":
            case "long":
            case "uint":
            case "Guid":
                return true;
        }

        return false;
    }

    /// <summary>Writes a primitive types.</summary>
    /// <param name="primitives">   The primitives.</param>
    /// <param name="writtenModels">[in,out] The written models.</param>
    /// <param name="subset"></param>
    private void WritePrimitiveTypes(
        IEnumerable<StructureDefinition> primitives,
        ref Dictionary<string, WrittenModelInfo> writtenModels,
        GenSubset subset)
    {
        // The FHIR primitives are all part of the base subset.
        if (subset is not GenSubset.Base) return;

        foreach (StructureDefinition primitive in primitives)
        {
            if (_exclusionSet.Contains(primitive.Name))
            {
                continue;
            }

            WritePrimitiveType(primitive, ref writtenModels);
        }
    }

    /// <summary>Writes a primitive type.</summary>
    /// <param name="primitive">    The primitive.</param>
    /// <param name="writtenModels">[in,out] The written models.</param>
    private void WritePrimitiveType(
        StructureDefinition primitive,
        ref Dictionary<string, WrittenModelInfo> writtenModels)
    {
        string exportName;
        string typeName;

        if (CSharpFirelyCommon.TypeNameMappings.ContainsKey(primitive.Name))
        {
            exportName = CSharpFirelyCommon.TypeNameMappings[primitive.Name];
        }
        else
        {
            exportName = primitive.Name.ToPascalCase();
        }

        if (CSharpFirelyCommon.PrimitiveTypeMap.ContainsKey(primitive.Name))
        {
            typeName = CSharpFirelyCommon.PrimitiveTypeMap[primitive.Name];
        }
        else
        {
            typeName = primitive.cgpBaseTypeName();
        }

        writtenModels.Add(
            primitive.Name,
            new WrittenModelInfo()
            {
                FhirName = primitive.Name,
                CsName = $"{Namespace}.{exportName}",
                IsAbstract = false,   // no abstract primitives
            });

        string filename = Path.Combine(_exportDirectory, "Generated", $"{exportName}.cs");

        _modelWriter.WriteLineIndented($"// {exportName}.cs");

        using (FileStream stream = new FileStream(filename, FileMode.Create))
        using (ExportStreamWriter writer = new ExportStreamWriter(stream))
        {
            _writer = writer;

            WriteHeaderPrimitive();

            WriteNamespaceOpen();

            if (!string.IsNullOrEmpty(primitive.cgpComment()))
            {
                WriteIndentedComment($"Primitive Type {primitive.Name}\n{primitive.cgpComment()}");
            }
            else
            {
                WriteIndentedComment($"Primitive Type {primitive.Name}");
            }

            _writer.WriteLineIndented("[System.Diagnostics.DebuggerDisplay(@\"\\{Value={Value}}\")]");
            WriteSerializable();

            string fhirTypeConstructor = $"\"{primitive.Name}\",\"{primitive.Url}\"";
            _writer.WriteLineIndented($"[FhirType({fhirTypeConstructor})]");

            _writer.WriteLineIndented(
                $"public partial class" +
                    $" {exportName}" +
                    $" : PrimitiveType, " +
                    PrimitiveValueInterface(typeName));

            // open class
            OpenScope();

            WritePropertyTypeName(primitive.Name);

            if (!string.IsNullOrEmpty(primitive.cgpValidationRegEx()))
            {
                WriteIndentedComment(
                    $"Must conform to the pattern \"{primitive.cgpValidationRegEx()}\"",
                    false);

                _writer.WriteLineIndented($"public const string PATTERN = @\"{primitive.cgpValidationRegEx()}\";");
                _writer.WriteLine(string.Empty);
            }

            _writer.WriteLineIndented($"public {exportName}({typeName} value)");
            OpenScope();
            _writer.WriteLineIndented("Value = value;");
            CloseScope();

            _writer.WriteLineIndented($"public {exportName}(): this(({typeName})null) {{}}");
            _writer.WriteLine(string.Empty);

            WriteIndentedComment("Primitive value of the element");

            _writer.WriteLineIndented("[FhirElement(\"value\", IsPrimitiveValue=true, XmlSerialization=XmlRepresentation.XmlAttr, InSummary=true, Order=30)]");
            _writer.WriteLineIndented($"[DeclaredType(Type = typeof({getSystemTypeForFhirType(primitive.Name)}))]");

            if (CSharpFirelyCommon.PrimitiveValidationPatterns.ContainsKey(primitive.Name))
            {
                _writer.WriteLineIndented($"[{CSharpFirelyCommon.PrimitiveValidationPatterns[primitive.Name]}]");
            }

            _writer.WriteLineIndented("[DataMember]");
            _writer.WriteLineIndented($"public {typeName} Value");
            OpenScope();
            _writer.WriteLineIndented($"get {{ return ({typeName})ObjectValue; }}");
            _writer.WriteLineIndented("set { ObjectValue = value; OnPropertyChanged(\"Value\"); }");
            CloseScope();

            /* Generate validator for simple string-based values that have a regex to validate them.
             * Skip validator for some types for which we have more performant, hand-written validators.
             */
        //if (!string.IsNullOrEmpty(primitive.ValidationRegEx) &&
        //    exportName != "FhirString" && exportName != "FhirUri" && exportName != "Markdown")
        //{
        //    _writer.WriteLineIndented("public static bool IsValidValue(string value) => Regex.IsMatch(value, \"^\" + PATTERN + \"$\", RegexOptions.Singleline);");
        //    _writer.WriteLine(string.Empty);
        //}

        // close class
        CloseScope();

            WriteNamespaceClose();

            WriteFooter();
        }
    }

    private string getSystemTypeForFhirType(string fhirType)
    {
        var systemTypeName = fhirType switch
        {
            "boolean" => "Boolean",
            "integer" => "Integer",
            "unsignedInt" => "Integer",
            "positiveInt" => "Integer",
            "integer64" => "Long",
            "time" => "Time",
            "date" => "Date",
            "instant" => "DateTime",
            "dateTime" => "DateTime",
            "decimal" => "Decimal",
            _ => "String"
        };

        return "SystemPrimitive." + systemTypeName;
    }

    private void WriteSerializable()
    {
        _writer.WriteLineIndented("[Serializable]");
        _writer.WriteLineIndented("[DataContract]");
    }

    private static string PrimitiveValueInterface(string valueType)
    {
        if (valueType.EndsWith("?", StringComparison.InvariantCulture))
        {
            string nullableType = valueType.TrimEnd('?');
            return $"INullableValue<{nullableType}>";
        }
        else
        {
            return $"IValue<{valueType}>";
        }
    }

    /// <summary>Writes the namespace open.</summary>
    private void WriteNamespaceOpen()
    {
        _writer.WriteLineIndented($"namespace {Namespace}");
        OpenScope();
    }

    /// <summary>Writes the namespace close.</summary>
    private void WriteNamespaceClose()
    {
        CloseScope();
    }

    /// <summary>Writes a header.</summary>
    private void WriteHeaderBasic()
    {
        WriteGenerationComment();

        _writer.WriteLineIndented("using Hl7.Fhir.Utility;");
        _writer.WriteLine(string.Empty);

        WriteCopyright();
    }

    /// <summary>Writes a header.</summary>
    private void WriteHeaderComplexDataType()
    {
        WriteGenerationComment();

        _writer.WriteLineIndented("using System;");
        _writer.WriteLineIndented("using System.Collections.Generic;");
        _writer.WriteLineIndented("using System.Linq;");
        _writer.WriteLineIndented("using System.Runtime.Serialization;");
        _writer.WriteLineIndented("using Hl7.Fhir.Introspection;");
        _writer.WriteLineIndented("using Hl7.Fhir.Serialization;");
        _writer.WriteLineIndented("using Hl7.Fhir.Specification;");
        _writer.WriteLineIndented("using Hl7.Fhir.Utility;");
        _writer.WriteLineIndented("using Hl7.Fhir.Validation;");
        _writer.WriteLine(string.Empty);

        WriteCopyright();

#if DISABLED    // 2020.07.01 - should be exporting everything with necessary summary tags
            _writer.WriteLineI("#pragma warning disable 1591 // suppress XML summary warnings ");
            _writer.WriteLine(string.Empty);
#endif
    }

    /// <summary>Writes a header above a primitive class.</summary>
    private void WriteHeaderPrimitive()
    {
        WriteGenerationComment();

        _writer.WriteLineIndented("using System;");
        _writer.WriteLineIndented("using System.Runtime.Serialization;");
        _writer.WriteLineIndented("using System.Text.RegularExpressions;");
        _writer.WriteLineIndented("using Hl7.Fhir.Introspection;");
        _writer.WriteLineIndented("using Hl7.Fhir.Specification;");
        _writer.WriteLineIndented("using Hl7.Fhir.Validation;");
        _writer.WriteLineIndented("using SystemPrimitive = Hl7.Fhir.ElementModel.Types;");
        _writer.WriteLine(string.Empty);

        WriteCopyright();
    }

    /// <summary>Writes the generation comment.</summary>
    /// <param name="writer">(Optional) The currently in-use text writer.</param>
    private void WriteGenerationComment(ExportStreamWriter? writer = null)
    {
        if (writer == null)
        {
            writer = _writer;
        }

        writer.WriteLineIndented("// <auto-generated/>");
        writer.WriteLineIndented($"// Contents of: {string.Join(", ", _info.Manifests.Select(kvp => kvp.Key))}");
        //writer.WriteLineIndented($"// Contents of: {_info.PackageName} version: {_info.VersionString}");

        if (_options.ExportKeys.Any())
        {
            string restrictions = string.Join("|", _options.ExportKeys);
            _writer.WriteLine($"  // Restricted to: {restrictions}");
        }

        writer.WriteLine(string.Empty);
    }

    /// <summary>Writes the copyright.</summary>
    private void WriteCopyright()
    {
        _writer.WriteLineIndented("/*");
        _writer.WriteLineIndented("  Copyright (c) 2011+, HL7, Inc.");
        _writer.WriteLineIndented("  All rights reserved.");
        _writer.WriteLineIndented("  ");
        _writer.WriteLineIndented("  Redistribution and use in source and binary forms, with or without modification, ");
        _writer.WriteLineIndented("  are permitted provided that the following conditions are met:");
        _writer.WriteLineIndented("  ");
        _writer.WriteLineIndented("   * Redistributions of source code must retain the above copyright notice, this ");
        _writer.WriteLineIndented("     list of conditions and the following disclaimer.");
        _writer.WriteLineIndented("   * Redistributions in binary form must reproduce the above copyright notice, ");
        _writer.WriteLineIndented("     this list of conditions and the following disclaimer in the documentation ");
        _writer.WriteLineIndented("     and/or other materials provided with the distribution.");
        _writer.WriteLineIndented("   * Neither the name of HL7 nor the names of its contributors may be used to ");
        _writer.WriteLineIndented("     endorse or promote products derived from this software without specific ");
        _writer.WriteLineIndented("     prior written permission.");
        _writer.WriteLineIndented("  ");
        _writer.WriteLineIndented("  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\" AND ");
        _writer.WriteLineIndented("  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED ");
        _writer.WriteLineIndented("  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. ");
        _writer.WriteLineIndented("  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, ");
        _writer.WriteLineIndented("  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT ");
        _writer.WriteLineIndented("  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR ");
        _writer.WriteLineIndented("  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, ");
        _writer.WriteLineIndented("  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ");
        _writer.WriteLineIndented("  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE ");
        _writer.WriteLineIndented("  POSSIBILITY OF SUCH DAMAGE.");
        _writer.WriteLineIndented("  ");
        _writer.WriteLineIndented("*/");
        _writer.WriteLine(string.Empty);
    }

    /// <summary>Writes a footer.</summary>
    private void WriteFooter()
    {
        WriteIndentedComment("end of file", singleLine: true);
    }

    /// <summary>Opens the scope.</summary>
    private void OpenScope()
        => CSharpFirelyCommon.OpenScope(_writer);

    /// <summary>Closes the scope.</summary>
    private void CloseScope(bool includeSemicolon = false, bool suppressNewline = false)
        => CSharpFirelyCommon.CloseScope(_writer, includeSemicolon, suppressNewline);

    /// <summary>Writes an indented comment.</summary>
    /// <param name="value">    The value.</param>
    /// <param name="isSummary">(Optional) True if is summary, false if not.</param>
    /// <param name="singleLine"></param>
    private void WriteIndentedComment(string value, bool isSummary = true, bool singleLine = false)
        => _writer.WriteIndentedComment(value, isSummary, singleLine);

    /// <summary>Adds a set of FhirTypes to a total set of exportable WrittenModelInfos.</summary>
    private static void AddModels(
       Dictionary<string, WrittenModelInfo> total,
       IEnumerable<WrittenModelInfo> typesToAdd)
    {
        foreach (WrittenModelInfo type in typesToAdd)
        {
            if (total.ContainsKey(type.FhirName))
            {
                continue;
            }

            total.Add(type.FhirName, type);
        }
    }

    private static void AddModels(
        Dictionary<string, WrittenModelInfo> total,
        IEnumerable<StructureDefinition> typesToAdd)
    {
        AddModels(total, typesToAdd.Select(ta => CreateWMI(ta)));

        WrittenModelInfo CreateWMI(StructureDefinition t)
        {
            string exportName = CSharpFirelyCommon.TypeNameMappings.ContainsKey(t.Name) ?
                CSharpFirelyCommon.TypeNameMappings[t.Name] :
                t.Name.ToPascalCase();

            return new WrittenModelInfo()
            {
                FhirName = t.Name,
                CsName = $"{Namespace}.{exportName}",
                IsAbstract = t.Abstract == true,
            };
        }
    }

    /// <summary>Information about a written value set.</summary>
    private struct WrittenValueSetInfo
    {
        internal string ClassName;
        internal string ValueSetName;
    }

    /// <summary>Information about the written element.</summary>
    private class WrittenElementInfo
    {
        internal string? FhirElementName;
        internal string? PropertyName;
        internal string? PropertyType;
        internal bool IsPrimitive;
        internal string? PrimitiveHelperName;
        internal string? PrimitiveHelperType;
        internal bool IsList;
        internal bool IsChoice;
        internal string? ElementType;
        internal bool IsCodedEnum;
    }

    /// <summary>Information about the written model.</summary>
    private struct WrittenModelInfo
    {
        internal string FhirName;
        internal string CsName;
        internal bool IsAbstract;
    }
}