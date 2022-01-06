// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Codes providing the status of an observation.
  /// </summary>
  public static class ObservationStatusCodes
  {
    /// <summary>
    /// Subsequent to being Final, the observation has been modified subsequent.  This includes updates/new information and corrections.
    /// </summary>
    public static readonly Coding Amended = new Coding
    {
      Code = "amended",
      Display = "Amended",
      System = "http://hl7.org/fhir/observation-status"
    };
    /// <summary>
    /// The observation is unavailable because the measurement was not started or not completed (also sometimes called "aborted").
    /// </summary>
    public static readonly Coding Cancelled = new Coding
    {
      Code = "cancelled",
      Display = "Cancelled",
      System = "http://hl7.org/fhir/observation-status"
    };
    /// <summary>
    /// Subsequent to being Final, the observation has been modified to correct an error in the test result.
    /// </summary>
    public static readonly Coding Corrected = new Coding
    {
      Code = "corrected",
      Display = "Corrected",
      System = "http://hl7.org/fhir/observation-status"
    };
    /// <summary>
    /// The observation has been withdrawn following previous final release.  This electronic record should never have existed, though it is possible that real-world decisions were based on it. (If real-world activity has occurred, the status should be "cancelled" rather than "entered-in-error".).
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/observation-status"
    };
    /// <summary>
    /// The observation is complete and there are no further actions needed. Additional information such "released", "signed", etc would be represented using [Provenance](provenance.html) which provides not only the act but also the actors and dates and other related data. These act states would be associated with an observation status of `preliminary` until they are all completed and then a status of `final` would be applied.
    /// </summary>
    public static readonly Coding Final = new Coding
    {
      Code = "final",
      Display = "Final",
      System = "http://hl7.org/fhir/observation-status"
    };
    /// <summary>
    /// This is an initial or interim observation: data may be incomplete or unverified.
    /// </summary>
    public static readonly Coding Preliminary = new Coding
    {
      Code = "preliminary",
      Display = "Preliminary",
      System = "http://hl7.org/fhir/observation-status"
    };
    /// <summary>
    /// The existence of the observation is registered, but there is no result yet available.
    /// </summary>
    public static readonly Coding Registered = new Coding
    {
      Code = "registered",
      Display = "Registered",
      System = "http://hl7.org/fhir/observation-status"
    };
    /// <summary>
    /// The authoring/source system does not know which of the status values currently applies for this observation. Note: This concept is not to be used for "other" - one of the listed statuses is presumed to apply, but the authoring/source system does not know which.
    /// </summary>
    public static readonly Coding Unknown = new Coding
    {
      Code = "unknown",
      Display = "Unknown",
      System = "http://hl7.org/fhir/observation-status"
    };

    /// <summary>
    /// Literal for code: Amended
    /// </summary>
    public const string LiteralAmended = "amended";

    /// <summary>
    /// Literal for code: Cancelled
    /// </summary>
    public const string LiteralCancelled = "cancelled";

    /// <summary>
    /// Literal for code: Corrected
    /// </summary>
    public const string LiteralCorrected = "corrected";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: Final
    /// </summary>
    public const string LiteralFinal = "final";

    /// <summary>
    /// Literal for code: Preliminary
    /// </summary>
    public const string LiteralPreliminary = "preliminary";

    /// <summary>
    /// Literal for code: Registered
    /// </summary>
    public const string LiteralRegistered = "registered";

    /// <summary>
    /// Literal for code: Unknown
    /// </summary>
    public const string LiteralUnknown = "unknown";
  };
}
