// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The impact of the content of a message.
  /// </summary>
  public static class MessageSignificanceCategoryCodes
  {
    /// <summary>
    /// The message represents/requests a change that should not be processed more than once; e.g., making a booking for an appointment.
    /// </summary>
    public static readonly Coding Consequence = new Coding
    {
      Code = "consequence",
      Display = "Consequence",
      System = "http://hl7.org/fhir/message-significance-category"
    };
    /// <summary>
    /// The message represents a response to query for current information. Retrospective processing is wrong and/or wasteful.
    /// </summary>
    public static readonly Coding Currency = new Coding
    {
      Code = "currency",
      Display = "Currency",
      System = "http://hl7.org/fhir/message-significance-category"
    };
    /// <summary>
    /// The content is not necessarily intended to be current, and it can be reprocessed, though there may be version issues created by processing old notifications.
    /// </summary>
    public static readonly Coding Notification = new Coding
    {
      Code = "notification",
      Display = "Notification",
      System = "http://hl7.org/fhir/message-significance-category"
    };

    /// <summary>
    /// Literal for code: Consequence
    /// </summary>
    public const string LiteralConsequence = "consequence";

    /// <summary>
    /// Literal for code: Currency
    /// </summary>
    public const string LiteralCurrency = "currency";

    /// <summary>
    /// Literal for code: Notification
    /// </summary>
    public const string LiteralNotification = "notification";
  };
}
