// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// The type of operator to use for assertion.
  /// </summary>
  public static class AssertOperatorCodesCodes
  {
    /// <summary>
    /// Compare value string contains a known value.
    /// </summary>
    public static readonly Coding Contains = new Coding
    {
      Code = "contains",
      Display = "contains",
      System = "http://hl7.org/fhir/assert-operator-codes"
    };
    /// <summary>
    /// Compare value is empty.
    /// </summary>
    public static readonly Coding Empty = new Coding
    {
      Code = "empty",
      Display = "empty",
      System = "http://hl7.org/fhir/assert-operator-codes"
    };
    /// <summary>
    /// Default value. Equals comparison.
    /// </summary>
    public static readonly new Coding Equals = new Coding
    {
      Code = "equals",
      Display = "equals",
      System = "http://hl7.org/fhir/assert-operator-codes"
    };
    /// <summary>
    /// Evaluate the FHIRPath expression as a boolean condition.
    /// </summary>
    public static readonly Coding Evaluate = new Coding
    {
      Code = "eval",
      Display = "evaluate",
      System = "http://hl7.org/fhir/assert-operator-codes"
    };
    /// <summary>
    /// Compare value to be greater than a known value.
    /// </summary>
    public static readonly Coding GreaterThan = new Coding
    {
      Code = "greaterThan",
      Display = "greaterThan",
      System = "http://hl7.org/fhir/assert-operator-codes"
    };
    /// <summary>
    /// Compare value within a known set of values.
    /// </summary>
    public static readonly Coding VALIn = new Coding
    {
      Code = "in",
      Display = "in",
      System = "http://hl7.org/fhir/assert-operator-codes"
    };
    /// <summary>
    /// Compare value to be less than a known value.
    /// </summary>
    public static readonly Coding LessThan = new Coding
    {
      Code = "lessThan",
      Display = "lessThan",
      System = "http://hl7.org/fhir/assert-operator-codes"
    };
    /// <summary>
    /// Compare value string does not contain a known value.
    /// </summary>
    public static readonly Coding NotContains = new Coding
    {
      Code = "notContains",
      Display = "notContains",
      System = "http://hl7.org/fhir/assert-operator-codes"
    };
    /// <summary>
    /// Compare value is not empty.
    /// </summary>
    public static readonly Coding NotEmpty = new Coding
    {
      Code = "notEmpty",
      Display = "notEmpty",
      System = "http://hl7.org/fhir/assert-operator-codes"
    };
    /// <summary>
    /// Not equals comparison.
    /// </summary>
    public static readonly Coding NotEquals = new Coding
    {
      Code = "notEquals",
      Display = "notEquals",
      System = "http://hl7.org/fhir/assert-operator-codes"
    };
    /// <summary>
    /// Compare value not within a known set of values.
    /// </summary>
    public static readonly Coding NotIn = new Coding
    {
      Code = "notIn",
      Display = "notIn",
      System = "http://hl7.org/fhir/assert-operator-codes"
    };

    /// <summary>
    /// Literal for code: Contains
    /// </summary>
    public const string LiteralContains = "contains";

    /// <summary>
    /// Literal for code: Empty
    /// </summary>
    public const string LiteralEmpty = "empty";

    /// <summary>
    /// Literal for code: Equals
    /// </summary>
    public const string LiteralEquals = "equals";

    /// <summary>
    /// Literal for code: Evaluate
    /// </summary>
    public const string LiteralEvaluate = "eval";

    /// <summary>
    /// Literal for code: GreaterThan
    /// </summary>
    public const string LiteralGreaterThan = "greaterThan";

    /// <summary>
    /// Literal for code: VALIn
    /// </summary>
    public const string LiteralVALIn = "in";

    /// <summary>
    /// Literal for code: LessThan
    /// </summary>
    public const string LiteralLessThan = "lessThan";

    /// <summary>
    /// Literal for code: NotContains
    /// </summary>
    public const string LiteralNotContains = "notContains";

    /// <summary>
    /// Literal for code: NotEmpty
    /// </summary>
    public const string LiteralNotEmpty = "notEmpty";

    /// <summary>
    /// Literal for code: NotEquals
    /// </summary>
    public const string LiteralNotEquals = "notEquals";

    /// <summary>
    /// Literal for code: NotIn
    /// </summary>
    public const string LiteralNotIn = "notIn";
  };
}
