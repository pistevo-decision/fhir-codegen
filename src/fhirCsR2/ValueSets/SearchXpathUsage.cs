// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// How a search parameter relates to the set of elements returned by evaluating its xpath query.
  /// </summary>
  public static class SearchXpathUsageCodes
  {
    /// <summary>
    /// The search parameter is based on a spatial transform of the selected nodes, using physical distance from the middle.
    /// </summary>
    public static readonly Coding Distance = new Coding
    {
      Code = "distance",
      Display = "Distance",
      System = "http://hl7.org/fhir/search-xpath-usage"
    };
    /// <summary>
    /// The search parameter is based on a spatial transform of the selected nodes.
    /// </summary>
    public static readonly Coding Nearby = new Coding
    {
      Code = "nearby",
      Display = "Nearby",
      System = "http://hl7.org/fhir/search-xpath-usage"
    };
    /// <summary>
    /// The search parameter is derived directly from the selected nodes based on the type definitions.
    /// </summary>
    public static readonly Coding Normal = new Coding
    {
      Code = "normal",
      Display = "Normal",
      System = "http://hl7.org/fhir/search-xpath-usage"
    };
    /// <summary>
    /// The interpretation of the xpath statement is unknown (and can't be automated).
    /// </summary>
    public static readonly Coding Other = new Coding
    {
      Code = "other",
      Display = "Other",
      System = "http://hl7.org/fhir/search-xpath-usage"
    };
    /// <summary>
    /// The search parameter is derived by a phonetic transform from the selected nodes.
    /// </summary>
    public static readonly Coding Phonetic = new Coding
    {
      Code = "phonetic",
      Display = "Phonetic",
      System = "http://hl7.org/fhir/search-xpath-usage"
    };

    /// <summary>
    /// Literal for code: Distance
    /// </summary>
    public const string LiteralDistance = "distance";

    /// <summary>
    /// Literal for code: Nearby
    /// </summary>
    public const string LiteralNearby = "nearby";

    /// <summary>
    /// Literal for code: Normal
    /// </summary>
    public const string LiteralNormal = "normal";

    /// <summary>
    /// Literal for code: Other
    /// </summary>
    public const string LiteralOther = "other";

    /// <summary>
    /// Literal for code: Phonetic
    /// </summary>
    public const string LiteralPhonetic = "phonetic";
  };
}
