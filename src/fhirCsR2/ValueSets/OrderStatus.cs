// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// The status of the response to an order.
  /// </summary>
  public static class OrderStatusCodes
  {
    /// <summary>
    /// Processing the order was stopped because of some workflow/business logic reason.
    /// </summary>
    public static readonly Coding Aborted = new Coding
    {
      Code = "aborted",
      Display = "Aborted",
      System = "http://hl7.org/fhir/order-status"
    };
    /// <summary>
    /// The order has been accepted, and work is in progress.
    /// </summary>
    public static readonly Coding Accepted = new Coding
    {
      Code = "accepted",
      Display = "Accepted",
      System = "http://hl7.org/fhir/order-status"
    };
    /// <summary>
    /// Processing the order was halted at the initiators request.
    /// </summary>
    public static readonly Coding Cancelled = new Coding
    {
      Code = "cancelled",
      Display = "Cancelled",
      System = "http://hl7.org/fhir/order-status"
    };
    /// <summary>
    /// The order has been completed.
    /// </summary>
    public static readonly Coding Completed = new Coding
    {
      Code = "completed",
      Display = "Completed",
      System = "http://hl7.org/fhir/order-status"
    };
    /// <summary>
    /// The order was unable to be processed because of a technical error (i.e. unexpected error)
    /// </summary>
    public static readonly Coding Error = new Coding
    {
      Code = "error",
      Display = "Error",
      System = "http://hl7.org/fhir/order-status"
    };
    /// <summary>
    /// The order is known, but no processing has occurred at this time
    /// </summary>
    public static readonly Coding Pending = new Coding
    {
      Code = "pending",
      Display = "Pending",
      System = "http://hl7.org/fhir/order-status"
    };
    /// <summary>
    /// The order was rejected because of a workflow/business logic reason
    /// </summary>
    public static readonly Coding Rejected = new Coding
    {
      Code = "rejected",
      Display = "Rejected",
      System = "http://hl7.org/fhir/order-status"
    };
    /// <summary>
    /// The order has been cancelled and replaced by another.
    /// </summary>
    public static readonly Coding Replaced = new Coding
    {
      Code = "replaced",
      Display = "Replaced",
      System = "http://hl7.org/fhir/order-status"
    };
    /// <summary>
    /// The order is undergoing initial processing to determine whether it will be accepted (usually this involves human review)
    /// </summary>
    public static readonly Coding Review = new Coding
    {
      Code = "review",
      Display = "Review",
      System = "http://hl7.org/fhir/order-status"
    };

    /// <summary>
    /// Literal for code: Aborted
    /// </summary>
    public const string LiteralAborted = "aborted";

    /// <summary>
    /// Literal for code: Accepted
    /// </summary>
    public const string LiteralAccepted = "accepted";

    /// <summary>
    /// Literal for code: Cancelled
    /// </summary>
    public const string LiteralCancelled = "cancelled";

    /// <summary>
    /// Literal for code: Completed
    /// </summary>
    public const string LiteralCompleted = "completed";

    /// <summary>
    /// Literal for code: Error
    /// </summary>
    public const string LiteralError = "error";

    /// <summary>
    /// Literal for code: Pending
    /// </summary>
    public const string LiteralPending = "pending";

    /// <summary>
    /// Literal for code: Rejected
    /// </summary>
    public const string LiteralRejected = "rejected";

    /// <summary>
    /// Literal for code: Replaced
    /// </summary>
    public const string LiteralReplaced = "replaced";

    /// <summary>
    /// Literal for code: Review
    /// </summary>
    public const string LiteralReview = "review";
  };
}
