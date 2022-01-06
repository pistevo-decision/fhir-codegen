// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The status of the diagnostic report.
  /// </summary>
  public static class DiagnosticReportStatusCodes
  {
    /// <summary>
    /// Subsequent to being final, the report has been modified.  This includes any change in the results, diagnosis, narrative text, or other content of a report that has been issued.
    /// </summary>
    public static readonly Coding Amended = new Coding
    {
      Code = "amended",
      Display = "Amended",
      System = "http://hl7.org/fhir/diagnostic-report-status"
    };
    /// <summary>
    /// Subsequent to being final, the report has been modified by adding new content. The existing content is unchanged.
    /// </summary>
    public static readonly Coding Appended = new Coding
    {
      Code = "appended",
      Display = "Appended",
      System = "http://hl7.org/fhir/diagnostic-report-status"
    };
    /// <summary>
    /// The report is unavailable because the measurement was not started or not completed (also sometimes called "aborted").
    /// </summary>
    public static readonly Coding Cancelled = new Coding
    {
      Code = "cancelled",
      Display = "Cancelled",
      System = "http://hl7.org/fhir/diagnostic-report-status"
    };
    /// <summary>
    /// Subsequent to being final, the report has been modified  to correct an error in the report or referenced results.
    /// </summary>
    public static readonly Coding Corrected = new Coding
    {
      Code = "corrected",
      Display = "Corrected",
      System = "http://hl7.org/fhir/diagnostic-report-status"
    };
    /// <summary>
    /// The report has been withdrawn following a previous final release.  This electronic record should never have existed, though it is possible that real-world decisions were based on it. (If real-world activity has occurred, the status should be "cancelled" rather than "entered-in-error".).
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/diagnostic-report-status"
    };
    /// <summary>
    /// The report is complete and verified by an authorized person.
    /// </summary>
    public static readonly Coding Final = new Coding
    {
      Code = "final",
      Display = "Final",
      System = "http://hl7.org/fhir/diagnostic-report-status"
    };
    /// <summary>
    /// This is a partial (e.g. initial, interim or preliminary) report: data in the report may be incomplete or unverified.
    /// </summary>
    public static readonly Coding Partial = new Coding
    {
      Code = "partial",
      Display = "Partial",
      System = "http://hl7.org/fhir/diagnostic-report-status"
    };
    /// <summary>
    /// Verified early results are available, but not all  results are final.
    /// </summary>
    public static readonly Coding Preliminary = new Coding
    {
      Code = "preliminary",
      Display = "Preliminary",
      System = "http://hl7.org/fhir/diagnostic-report-status"
    };
    /// <summary>
    /// The existence of the report is registered, but there is nothing yet available.
    /// </summary>
    public static readonly Coding Registered = new Coding
    {
      Code = "registered",
      Display = "Registered",
      System = "http://hl7.org/fhir/diagnostic-report-status"
    };
    /// <summary>
    /// The authoring/source system does not know which of the status values currently applies for this observation. Note: This concept is not to be used for "other" - one of the listed statuses is presumed to apply, but the authoring/source system does not know which.
    /// </summary>
    public static readonly Coding Unknown = new Coding
    {
      Code = "unknown",
      Display = "Unknown",
      System = "http://hl7.org/fhir/diagnostic-report-status"
    };

    /// <summary>
    /// Literal for code: Amended
    /// </summary>
    public const string LiteralAmended = "amended";

    /// <summary>
    /// Literal for code: Appended
    /// </summary>
    public const string LiteralAppended = "appended";

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
    /// Literal for code: Partial
    /// </summary>
    public const string LiteralPartial = "partial";

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
