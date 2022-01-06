// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Whether a reference needs to be version specific or version independent, or whether either can be used
  /// </summary>
  public static class ReferenceVersionRulesCodes
  {
    /// <summary>
    /// The reference may be either version independent or version specific
    /// </summary>
    public static readonly Coding EitherSpecificOrIndependent = new Coding
    {
      Code = "either",
      Display = "Either Specific or independent",
      System = "http://hl7.org/fhir/reference-version-rules"
    };
    /// <summary>
    /// The reference must be version independent
    /// </summary>
    public static readonly Coding VersionIndependent = new Coding
    {
      Code = "independent",
      Display = "Version independent",
      System = "http://hl7.org/fhir/reference-version-rules"
    };
    /// <summary>
    /// The reference must be version specific
    /// </summary>
    public static readonly Coding VersionSpecific = new Coding
    {
      Code = "specific",
      Display = "Version Specific",
      System = "http://hl7.org/fhir/reference-version-rules"
    };

    /// <summary>
    /// Literal for code: EitherSpecificOrIndependent
    /// </summary>
    public const string LiteralEitherSpecificOrIndependent = "either";

    /// <summary>
    /// Literal for code: VersionIndependent
    /// </summary>
    public const string LiteralVersionIndependent = "independent";

    /// <summary>
    /// Literal for code: VersionSpecific
    /// </summary>
    public const string LiteralVersionSpecific = "specific";
  };
}
