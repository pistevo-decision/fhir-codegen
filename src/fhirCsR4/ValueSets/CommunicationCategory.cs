// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Codes for general categories of communications such as alerts, instructions, etc.
  /// </summary>
  public static class CommunicationCategoryCodes
  {
    /// <summary>
    /// The communication conveys an alert.
    /// </summary>
    public static readonly Coding Alert = new Coding
    {
      Code = "alert",
      Display = "Alert",
      System = "http://terminology.hl7.org/CodeSystem/communication-category"
    };
    /// <summary>
    /// The communication conveys an instruction.
    /// </summary>
    public static readonly Coding Instruction = new Coding
    {
      Code = "instruction",
      Display = "Instruction",
      System = "http://terminology.hl7.org/CodeSystem/communication-category"
    };
    /// <summary>
    /// The communication conveys a notification.
    /// </summary>
    public static readonly Coding Notification = new Coding
    {
      Code = "notification",
      Display = "Notification",
      System = "http://terminology.hl7.org/CodeSystem/communication-category"
    };
    /// <summary>
    /// The communication conveys a reminder.
    /// </summary>
    public static readonly Coding Reminder = new Coding
    {
      Code = "reminder",
      Display = "Reminder",
      System = "http://terminology.hl7.org/CodeSystem/communication-category"
    };

    /// <summary>
    /// Literal for code: Alert
    /// </summary>
    public const string LiteralAlert = "alert";

    /// <summary>
    /// Literal for code: Instruction
    /// </summary>
    public const string LiteralInstruction = "instruction";

    /// <summary>
    /// Literal for code: Notification
    /// </summary>
    public const string LiteralNotification = "notification";

    /// <summary>
    /// Literal for code: Reminder
    /// </summary>
    public const string LiteralReminder = "reminder";
  };
}
