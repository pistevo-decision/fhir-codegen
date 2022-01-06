// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// Indicates whether the plan is currently being acted upon, represents future intentions or is now a historical record.
  /// </summary>
  public static class CarePlanStatusCodes
  {
    /// <summary>
    /// The plan is intended to be followed and used as part of patient care.
    /// </summary>
    public static readonly Coding Active = new Coding
    {
      Code = "active",
      Display = "Active",
      System = "http://hl7.org/fhir/care-plan-status"
    };
    /// <summary>
    /// The plan has been terminated prior to reaching completion (though it may have been replaced by a new plan).
    /// </summary>
    public static readonly Coding Cancelled = new Coding
    {
      Code = "cancelled",
      Display = "Cancelled",
      System = "http://hl7.org/fhir/care-plan-status"
    };
    /// <summary>
    /// The plan is no longer in use and is not expected to be followed or used in patient care.
    /// </summary>
    public static readonly Coding Completed = new Coding
    {
      Code = "completed",
      Display = "Completed",
      System = "http://hl7.org/fhir/care-plan-status"
    };
    /// <summary>
    /// The plan is in development or awaiting use but is not yet intended to be acted upon.
    /// </summary>
    public static readonly Coding Pending = new Coding
    {
      Code = "draft",
      Display = "Pending",
      System = "http://hl7.org/fhir/care-plan-status"
    };
    /// <summary>
    /// The plan has been suggested but no commitment to it has yet been made.
    /// </summary>
    public static readonly Coding Proposed = new Coding
    {
      Code = "proposed",
      Display = "Proposed",
      System = "http://hl7.org/fhir/care-plan-status"
    };

    /// <summary>
    /// Literal for code: Active
    /// </summary>
    public const string LiteralActive = "active";

    /// <summary>
    /// Literal for code: Cancelled
    /// </summary>
    public const string LiteralCancelled = "cancelled";

    /// <summary>
    /// Literal for code: Completed
    /// </summary>
    public const string LiteralCompleted = "completed";

    /// <summary>
    /// Literal for code: Pending
    /// </summary>
    public const string LiteralPending = "draft";

    /// <summary>
    /// Literal for code: Proposed
    /// </summary>
    public const string LiteralProposed = "proposed";
  };
}
