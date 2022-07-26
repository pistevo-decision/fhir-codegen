// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// The current status of the task.
  /// </summary>
  public static class TaskStatusCodes
  {
    /// <summary>
    /// The potential performer has agreed to execute the task but has not yet started work.
    /// </summary>
    public static readonly Coding Accepted = new Coding
    {
      Code = "accepted",
      Display = "Accepted",
      System = "http://hl7.org/fhir/task-status"
    };
    /// <summary>
    /// The task was not completed.
    /// </summary>
    public static readonly Coding Cancelled = new Coding
    {
      Code = "cancelled",
      Display = "Cancelled",
      System = "http://hl7.org/fhir/task-status"
    };
    /// <summary>
    /// The task has been completed.
    /// </summary>
    public static readonly Coding Completed = new Coding
    {
      Code = "completed",
      Display = "Completed",
      System = "http://hl7.org/fhir/task-status"
    };
    /// <summary>
    /// The task is not yet ready to be acted upon.
    /// </summary>
    public static readonly Coding Draft = new Coding
    {
      Code = "draft",
      Display = "Draft",
      System = "http://hl7.org/fhir/task-status"
    };
    /// <summary>
    /// The task should never have existed and is retained only because of the possibility it may have used.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/task-status"
    };
    /// <summary>
    /// The task was attempted but could not be completed due to some error.
    /// </summary>
    public static readonly Coding Failed = new Coding
    {
      Code = "failed",
      Display = "Failed",
      System = "http://hl7.org/fhir/task-status"
    };
    /// <summary>
    /// The task has been started but is not yet complete.
    /// </summary>
    public static readonly Coding InProgress = new Coding
    {
      Code = "in-progress",
      Display = "In Progress",
      System = "http://hl7.org/fhir/task-status"
    };
    /// <summary>
    /// The task has been started but work has been paused.
    /// </summary>
    public static readonly Coding OnHold = new Coding
    {
      Code = "on-hold",
      Display = "On Hold",
      System = "http://hl7.org/fhir/task-status"
    };
    /// <summary>
    /// The task is ready to be performed, but no action has yet been taken.  Used in place of requested/received/accepted/rejected when request assignment and acceptance is a given.
    /// </summary>
    public static readonly Coding Ready = new Coding
    {
      Code = "ready",
      Display = "Ready",
      System = "http://hl7.org/fhir/task-status"
    };
    /// <summary>
    /// A potential performer has claimed ownership of the task and is evaluating whether to perform it.
    /// </summary>
    public static readonly Coding Received = new Coding
    {
      Code = "received",
      Display = "Received",
      System = "http://hl7.org/fhir/task-status"
    };
    /// <summary>
    /// The potential performer who claimed ownership of the task has decided not to execute it prior to performing any action.
    /// </summary>
    public static readonly Coding Rejected = new Coding
    {
      Code = "rejected",
      Display = "Rejected",
      System = "http://hl7.org/fhir/task-status"
    };
    /// <summary>
    /// The task is ready to be acted upon and action is sought.
    /// </summary>
    public static readonly Coding Requested = new Coding
    {
      Code = "requested",
      Display = "Requested",
      System = "http://hl7.org/fhir/task-status"
    };

    /// <summary>
    /// Literal for code: Accepted
    /// </summary>
    public const string LiteralAccepted = "accepted";

    /// <summary>
    /// Literal for code: TaskStatusAccepted
    /// </summary>
    public const string LiteralTaskStatusAccepted = "http://hl7.org/fhir/task-status#accepted";

    /// <summary>
    /// Literal for code: Cancelled
    /// </summary>
    public const string LiteralCancelled = "cancelled";

    /// <summary>
    /// Literal for code: TaskStatusCancelled
    /// </summary>
    public const string LiteralTaskStatusCancelled = "http://hl7.org/fhir/task-status#cancelled";

    /// <summary>
    /// Literal for code: Completed
    /// </summary>
    public const string LiteralCompleted = "completed";

    /// <summary>
    /// Literal for code: TaskStatusCompleted
    /// </summary>
    public const string LiteralTaskStatusCompleted = "http://hl7.org/fhir/task-status#completed";

    /// <summary>
    /// Literal for code: Draft
    /// </summary>
    public const string LiteralDraft = "draft";

    /// <summary>
    /// Literal for code: TaskStatusDraft
    /// </summary>
    public const string LiteralTaskStatusDraft = "http://hl7.org/fhir/task-status#draft";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: TaskStatusEnteredInError
    /// </summary>
    public const string LiteralTaskStatusEnteredInError = "http://hl7.org/fhir/task-status#entered-in-error";

    /// <summary>
    /// Literal for code: Failed
    /// </summary>
    public const string LiteralFailed = "failed";

    /// <summary>
    /// Literal for code: TaskStatusFailed
    /// </summary>
    public const string LiteralTaskStatusFailed = "http://hl7.org/fhir/task-status#failed";

    /// <summary>
    /// Literal for code: InProgress
    /// </summary>
    public const string LiteralInProgress = "in-progress";

    /// <summary>
    /// Literal for code: TaskStatusInProgress
    /// </summary>
    public const string LiteralTaskStatusInProgress = "http://hl7.org/fhir/task-status#in-progress";

    /// <summary>
    /// Literal for code: OnHold
    /// </summary>
    public const string LiteralOnHold = "on-hold";

    /// <summary>
    /// Literal for code: TaskStatusOnHold
    /// </summary>
    public const string LiteralTaskStatusOnHold = "http://hl7.org/fhir/task-status#on-hold";

    /// <summary>
    /// Literal for code: Ready
    /// </summary>
    public const string LiteralReady = "ready";

    /// <summary>
    /// Literal for code: TaskStatusReady
    /// </summary>
    public const string LiteralTaskStatusReady = "http://hl7.org/fhir/task-status#ready";

    /// <summary>
    /// Literal for code: Received
    /// </summary>
    public const string LiteralReceived = "received";

    /// <summary>
    /// Literal for code: TaskStatusReceived
    /// </summary>
    public const string LiteralTaskStatusReceived = "http://hl7.org/fhir/task-status#received";

    /// <summary>
    /// Literal for code: Rejected
    /// </summary>
    public const string LiteralRejected = "rejected";

    /// <summary>
    /// Literal for code: TaskStatusRejected
    /// </summary>
    public const string LiteralTaskStatusRejected = "http://hl7.org/fhir/task-status#rejected";

    /// <summary>
    /// Literal for code: Requested
    /// </summary>
    public const string LiteralRequested = "requested";

    /// <summary>
    /// Literal for code: TaskStatusRequested
    /// </summary>
    public const string LiteralTaskStatusRequested = "http://hl7.org/fhir/task-status#requested";

    /// <summary>
    /// Dictionary for looking up TaskStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "accepted", Accepted }, 
      { "http://hl7.org/fhir/task-status#accepted", Accepted }, 
      { "cancelled", Cancelled }, 
      { "http://hl7.org/fhir/task-status#cancelled", Cancelled }, 
      { "completed", Completed }, 
      { "http://hl7.org/fhir/task-status#completed", Completed }, 
      { "draft", Draft }, 
      { "http://hl7.org/fhir/task-status#draft", Draft }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/task-status#entered-in-error", EnteredInError }, 
      { "failed", Failed }, 
      { "http://hl7.org/fhir/task-status#failed", Failed }, 
      { "in-progress", InProgress }, 
      { "http://hl7.org/fhir/task-status#in-progress", InProgress }, 
      { "on-hold", OnHold }, 
      { "http://hl7.org/fhir/task-status#on-hold", OnHold }, 
      { "ready", Ready }, 
      { "http://hl7.org/fhir/task-status#ready", Ready }, 
      { "received", Received }, 
      { "http://hl7.org/fhir/task-status#received", Received }, 
      { "rejected", Rejected }, 
      { "http://hl7.org/fhir/task-status#rejected", Rejected }, 
      { "requested", Requested }, 
      { "http://hl7.org/fhir/task-status#requested", Requested }, 
    };
  };
}