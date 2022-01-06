// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// How the referenced structure is used in this mapping
  /// </summary>
  public static class MapModelModeCodes
  {
    /// <summary>
    /// This structure describes an instance that the mapping engine may ask to create that is used a target of data
    /// </summary>
    public static readonly Coding ProducedStructureDefinition = new Coding
    {
      Code = "produced",
      Display = "Produced Structure Definition",
      System = "http://hl7.org/fhir/map-model-mode"
    };
    /// <summary>
    /// This structure describes an instance that the mapping engine may ask for that is used a source of data
    /// </summary>
    public static readonly Coding QueriedStructureDefinition = new Coding
    {
      Code = "queried",
      Display = "Queried Structure Definition",
      System = "http://hl7.org/fhir/map-model-mode"
    };
    /// <summary>
    /// This structure describes an instance passed to the mapping engine that is used a source of data
    /// </summary>
    public static readonly Coding SourceStructureDefinition = new Coding
    {
      Code = "source",
      Display = "Source Structure Definition",
      System = "http://hl7.org/fhir/map-model-mode"
    };
    /// <summary>
    /// This structure describes an instance passed to the mapping engine that is used a target of data
    /// </summary>
    public static readonly Coding TargetStructureDefinition = new Coding
    {
      Code = "target",
      Display = "Target Structure Definition",
      System = "http://hl7.org/fhir/map-model-mode"
    };

    /// <summary>
    /// Literal for code: ProducedStructureDefinition
    /// </summary>
    public const string LiteralProducedStructureDefinition = "produced";

    /// <summary>
    /// Literal for code: QueriedStructureDefinition
    /// </summary>
    public const string LiteralQueriedStructureDefinition = "queried";

    /// <summary>
    /// Literal for code: SourceStructureDefinition
    /// </summary>
    public const string LiteralSourceStructureDefinition = "source";

    /// <summary>
    /// Literal for code: TargetStructureDefinition
    /// </summary>
    public const string LiteralTargetStructureDefinition = "target";
  };
}
