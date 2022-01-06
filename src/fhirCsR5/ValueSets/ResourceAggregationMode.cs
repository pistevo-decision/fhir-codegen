// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// How resource references can be aggregated.
  /// </summary>
  public static class ResourceAggregationModeCodes
  {
    /// <summary>
    /// The resource the reference points to will be found in the same bundle as the resource that includes the reference.
    /// </summary>
    public static readonly Coding Bundled = new Coding
    {
      Code = "bundled",
      Display = "Bundled",
      System = "http://hl7.org/fhir/resource-aggregation-mode"
    };
    /// <summary>
    /// The reference is a local reference to a contained resource.
    /// </summary>
    public static readonly Coding Contained = new Coding
    {
      Code = "contained",
      Display = "Contained",
      System = "http://hl7.org/fhir/resource-aggregation-mode"
    };
    /// <summary>
    /// The reference to a resource that has to be resolved externally to the resource that includes the reference.
    /// </summary>
    public static readonly Coding Referenced = new Coding
    {
      Code = "referenced",
      Display = "Referenced",
      System = "http://hl7.org/fhir/resource-aggregation-mode"
    };

    /// <summary>
    /// Literal for code: Bundled
    /// </summary>
    public const string LiteralBundled = "bundled";

    /// <summary>
    /// Literal for code: Contained
    /// </summary>
    public const string LiteralContained = "contained";

    /// <summary>
    /// Literal for code: Referenced
    /// </summary>
    public const string LiteralReferenced = "referenced";
  };
}
