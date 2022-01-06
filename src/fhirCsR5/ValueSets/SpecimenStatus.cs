// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Codes providing the status/availability of a specimen.
  /// </summary>
  public static class SpecimenStatusCodes
  {
    /// <summary>
    /// The physical specimen is present and in good condition.
    /// </summary>
    public static readonly Coding Available = new Coding
    {
      Code = "available",
      Display = "Available",
      System = "http://hl7.org/fhir/specimen-status"
    };
    /// <summary>
    /// The specimen was entered in error and therefore nullified.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/specimen-status"
    };
    /// <summary>
    /// There is no physical specimen because it is either lost, destroyed or consumed.
    /// </summary>
    public static readonly Coding Unavailable = new Coding
    {
      Code = "unavailable",
      Display = "Unavailable",
      System = "http://hl7.org/fhir/specimen-status"
    };
    /// <summary>
    /// The specimen cannot be used because of a quality issue such as a broken container, contamination, or too old.
    /// </summary>
    public static readonly Coding Unsatisfactory = new Coding
    {
      Code = "unsatisfactory",
      Display = "Unsatisfactory",
      System = "http://hl7.org/fhir/specimen-status"
    };

    /// <summary>
    /// Literal for code: Available
    /// </summary>
    public const string LiteralAvailable = "available";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: Unavailable
    /// </summary>
    public const string LiteralUnavailable = "unavailable";

    /// <summary>
    /// Literal for code: Unsatisfactory
    /// </summary>
    public const string LiteralUnsatisfactory = "unsatisfactory";
  };
}
