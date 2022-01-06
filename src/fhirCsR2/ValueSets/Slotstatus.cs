// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// The free/busy status of a slot.
  /// </summary>
  public static class SlotstatusCodes
  {
    /// <summary>
    /// Indicates that the time interval is busy because one  or more events have been scheduled for that interval.
    /// </summary>
    public static readonly Coding Busy = new Coding
    {
      Code = "busy",
      Display = "Busy",
      System = "http://hl7.org/fhir/slotstatus"
    };
    /// <summary>
    /// Indicates that the time interval is busy because one or more events have been tentatively scheduled for that interval.
    /// </summary>
    public static readonly Coding BusyTentative = new Coding
    {
      Code = "busy-tentative",
      Display = "Busy (Tentative)",
      System = "http://hl7.org/fhir/slotstatus"
    };
    /// <summary>
    /// Indicates that the time interval is busy and that the interval can not be scheduled.
    /// </summary>
    public static readonly Coding BusyUnavailable = new Coding
    {
      Code = "busy-unavailable",
      Display = "Busy (Unavailable)",
      System = "http://hl7.org/fhir/slotstatus"
    };
    /// <summary>
    /// Indicates that the time interval is free for scheduling.
    /// </summary>
    public static readonly Coding Free = new Coding
    {
      Code = "free",
      Display = "Free",
      System = "http://hl7.org/fhir/slotstatus"
    };

    /// <summary>
    /// Literal for code: Busy
    /// </summary>
    public const string LiteralBusy = "busy";

    /// <summary>
    /// Literal for code: BusyTentative
    /// </summary>
    public const string LiteralBusyTentative = "busy-tentative";

    /// <summary>
    /// Literal for code: BusyUnavailable
    /// </summary>
    public const string LiteralBusyUnavailable = "busy-unavailable";

    /// <summary>
    /// Literal for code: Free
    /// </summary>
    public const string LiteralFree = "free";
  };
}
