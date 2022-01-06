// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Mode for this instance of data.
  /// </summary>
  public static class MapInputModeCodes
  {
    /// <summary>
    /// Names an input instance used a source for mapping.
    /// </summary>
    public static readonly Coding SourceInstance = new Coding
    {
      Code = "source",
      Display = "Source Instance",
      System = "http://hl7.org/fhir/map-input-mode"
    };
    /// <summary>
    /// Names an instance that is being populated.
    /// </summary>
    public static readonly Coding TargetInstance = new Coding
    {
      Code = "target",
      Display = "Target Instance",
      System = "http://hl7.org/fhir/map-input-mode"
    };

    /// <summary>
    /// Literal for code: SourceInstance
    /// </summary>
    public const string LiteralSourceInstance = "source";

    /// <summary>
    /// Literal for code: TargetInstance
    /// </summary>
    public const string LiteralTargetInstance = "target";
  };
}
