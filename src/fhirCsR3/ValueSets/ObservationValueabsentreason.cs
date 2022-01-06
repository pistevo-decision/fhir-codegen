// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// This value set defines the set of codes for identifying the reason why the expected result in Observation.value[x] is missing.
  /// </summary>
  public static class ObservationValueabsentreasonCodes
  {
    /// <summary>
    /// The source human does not know the value.
    /// </summary>
    public static readonly Coding Asked = new Coding
    {
      Code = "asked",
      Display = "Asked",
      System = "http://hl7.org/fhir/data-absent-reason"
    };
    /// <summary>
    /// The content of the data is represented in the resource narrative.
    /// </summary>
    public static readonly Coding AsText = new Coding
    {
      Code = "astext",
      Display = "As Text",
      System = "http://hl7.org/fhir/data-absent-reason"
    };
    /// <summary>
    /// Some system or workflow process error means that the information is not available.
    /// </summary>
    public static readonly Coding Error = new Coding
    {
      Code = "error",
      Display = "Error",
      System = "http://hl7.org/fhir/data-absent-reason"
    };
    /// <summary>
    /// The information is not available due to security, privacy or related reasons.
    /// </summary>
    public static readonly Coding Masked = new Coding
    {
      Code = "masked",
      Display = "Masked",
      System = "http://hl7.org/fhir/data-absent-reason"
    };
    /// <summary>
    /// NaN, standing for not a number, is a numeric data type value representing an undefined or unrepresentable value.
    /// </summary>
    public static readonly Coding NotANumber = new Coding
    {
      Code = "NaN",
      Display = "Not a Number",
      System = "http://hl7.org/fhir/data-absent-reason"
    };
    /// <summary>
    /// The workflow didn't lead to this value being known.
    /// </summary>
    public static readonly Coding NotAsked = new Coding
    {
      Code = "not-asked",
      Display = "Not Asked",
      System = "http://hl7.org/fhir/data-absent-reason"
    };
    /// <summary>
    /// The value is not available because the observation procedure (test, etc.) was not performed.
    /// </summary>
    public static readonly Coding NotPerformed = new Coding
    {
      Code = "not-performed",
      Display = "Not Performed",
      System = "http://hl7.org/fhir/data-absent-reason"
    };
    /// <summary>
    /// There is reason to expect (from the workflow) that the value may become known.
    /// </summary>
    public static readonly Coding Temp = new Coding
    {
      Code = "temp",
      Display = "Temp",
      System = "http://hl7.org/fhir/data-absent-reason"
    };
    /// <summary>
    /// The value is not known.
    /// </summary>
    public static readonly Coding Unknown = new Coding
    {
      Code = "unknown",
      Display = "Unknown",
      System = "http://hl7.org/fhir/data-absent-reason"
    };
    /// <summary>
    /// The source system wasn't capable of supporting this element.
    /// </summary>
    public static readonly Coding Unsupported = new Coding
    {
      Code = "unsupported",
      Display = "Unsupported",
      System = "http://hl7.org/fhir/data-absent-reason"
    };

    /// <summary>
    /// Literal for code: Asked
    /// </summary>
    public const string LiteralAsked = "asked";

    /// <summary>
    /// Literal for code: AsText
    /// </summary>
    public const string LiteralAsText = "astext";

    /// <summary>
    /// Literal for code: Error
    /// </summary>
    public const string LiteralError = "error";

    /// <summary>
    /// Literal for code: Masked
    /// </summary>
    public const string LiteralMasked = "masked";

    /// <summary>
    /// Literal for code: NotANumber
    /// </summary>
    public const string LiteralNotANumber = "NaN";

    /// <summary>
    /// Literal for code: NotAsked
    /// </summary>
    public const string LiteralNotAsked = "not-asked";

    /// <summary>
    /// Literal for code: NotPerformed
    /// </summary>
    public const string LiteralNotPerformed = "not-performed";

    /// <summary>
    /// Literal for code: Temp
    /// </summary>
    public const string LiteralTemp = "temp";

    /// <summary>
    /// Literal for code: Unknown
    /// </summary>
    public const string LiteralUnknown = "unknown";

    /// <summary>
    /// Literal for code: Unsupported
    /// </summary>
    public const string LiteralUnsupported = "unsupported";
  };
}
