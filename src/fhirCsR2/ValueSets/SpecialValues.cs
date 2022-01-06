// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// A set of generally useful codes defined so they can be included in value sets.
  /// </summary>
  public static class SpecialValuesCodes
  {
    /// <summary>
    /// Boolean false.
    /// </summary>
    public static readonly Coding VALFalse = new Coding
    {
      Code = "false",
      Display = "false",
      System = "http://hl7.org/fhir/special-values"
    };
    /// <summary>
    /// The are no known applicable values in this context.
    /// </summary>
    public static readonly Coding NilKnown = new Coding
    {
      Code = "nil-known",
      Display = "Nil Known",
      System = "http://hl7.org/fhir/special-values"
    };
    /// <summary>
    /// The specific quantity is not known, but is known to be non-zero and is not specified because it makes up the bulk of the material.
    /// </summary>
    public static readonly Coding SufficientQuantity = new Coding
    {
      Code = "sufficient",
      Display = "Sufficient Quantity",
      System = "http://hl7.org/fhir/special-values"
    };
    /// <summary>
    /// The content is greater than zero, but too small to be quantified.
    /// </summary>
    public static readonly Coding TraceAmountDetected = new Coding
    {
      Code = "trace",
      Display = "Trace Amount Detected",
      System = "http://hl7.org/fhir/special-values"
    };
    /// <summary>
    /// Boolean true.
    /// </summary>
    public static readonly Coding VALTrue = new Coding
    {
      Code = "true",
      Display = "true",
      System = "http://hl7.org/fhir/special-values"
    };
    /// <summary>
    /// The value is no longer available.
    /// </summary>
    public static readonly Coding ValueWithdrawn = new Coding
    {
      Code = "withdrawn",
      Display = "Value Withdrawn",
      System = "http://hl7.org/fhir/special-values"
    };

    /// <summary>
    /// Literal for code: VALFalse
    /// </summary>
    public const string LiteralVALFalse = "false";

    /// <summary>
    /// Literal for code: NilKnown
    /// </summary>
    public const string LiteralNilKnown = "nil-known";

    /// <summary>
    /// Literal for code: SufficientQuantity
    /// </summary>
    public const string LiteralSufficientQuantity = "sufficient";

    /// <summary>
    /// Literal for code: TraceAmountDetected
    /// </summary>
    public const string LiteralTraceAmountDetected = "trace";

    /// <summary>
    /// Literal for code: VALTrue
    /// </summary>
    public const string LiteralVALTrue = "true";

    /// <summary>
    /// Literal for code: ValueWithdrawn
    /// </summary>
    public const string LiteralValueWithdrawn = "withdrawn";
  };
}
