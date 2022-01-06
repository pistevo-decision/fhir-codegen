// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// The kind of operation to perform as a part of a property based filter.
  /// </summary>
  public static class FilterOperatorCodes
  {
    /// <summary>
    /// The specified property of the code equals the provided value.
    /// </summary>
    public static readonly new Coding Equals = new Coding
    {
      Code = "=",
      Display = "Equals",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// The specified property of the code is in the set of codes or concepts specified in the provided value (comma separated list).
    /// </summary>
    public static readonly Coding InSet = new Coding
    {
      Code = "in",
      Display = "In Set",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// Includes all concept ids that have a transitive is-a relationship with the concept Id provided as the value, including the provided concept itself.
    /// </summary>
    public static readonly Coding IsABySubsumption = new Coding
    {
      Code = "is-a",
      Display = "Is A (by subsumption)",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// The specified property of the code does not have an is-a relationship with the provided value.
    /// </summary>
    public static readonly Coding NotIsABySubsumption = new Coding
    {
      Code = "is-not-a",
      Display = "Not (Is A) (by subsumption)",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// The specified property of the code is not in the set of codes or concepts specified in the provided value (comma separated list).
    /// </summary>
    public static readonly Coding NotInSet = new Coding
    {
      Code = "not-in",
      Display = "Not in Set",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// The specified property of the code  matches the regex specified in the provided value.
    /// </summary>
    public static readonly Coding RegularExpression = new Coding
    {
      Code = "regex",
      Display = "Regular Expression",
      System = "http://hl7.org/fhir/filter-operator"
    };

    /// <summary>
    /// Literal for code: Equals
    /// </summary>
    public const string LiteralEquals = "=";

    /// <summary>
    /// Literal for code: InSet
    /// </summary>
    public const string LiteralInSet = "in";

    /// <summary>
    /// Literal for code: IsABySubsumption
    /// </summary>
    public const string LiteralIsABySubsumption = "is-a";

    /// <summary>
    /// Literal for code: NotIsABySubsumption
    /// </summary>
    public const string LiteralNotIsABySubsumption = "is-not-a";

    /// <summary>
    /// Literal for code: NotInSet
    /// </summary>
    public const string LiteralNotInSet = "not-in";

    /// <summary>
    /// Literal for code: RegularExpression
    /// </summary>
    public const string LiteralRegularExpression = "regex";
  };
}
