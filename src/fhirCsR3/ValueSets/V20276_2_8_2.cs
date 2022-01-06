// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// FHIR Value set/code system definition for HL7 v2 table 0276 ( Appointment reason codes)
  /// </summary>
  public static class V20276Codes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding ARoutineCheckUpSuchAsAnAnnualPhysical = new Coding
    {
      Code = "CHECKUP",
      Display = "A routine check-up, such as an annual physical",
      System = "http://hl7.org/fhir/v2/0276"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding EmergencyAppointment = new Coding
    {
      Code = "EMERGENCY",
      Display = "Emergency appointment",
      System = "http://hl7.org/fhir/v2/0276"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AFollowUpVisitFromAPreviousAppointment = new Coding
    {
      Code = "FOLLOWUP",
      Display = "A follow up visit from a previous appointment",
      System = "http://hl7.org/fhir/v2/0276"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding RoutineAppointmentDefaultIfNotValued = new Coding
    {
      Code = "ROUTINE",
      Display = "Routine appointment - default if not valued",
      System = "http://hl7.org/fhir/v2/0276"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding APreviouslyUnscheduledWalkInVisit = new Coding
    {
      Code = "WALKIN",
      Display = "A previously unscheduled walk-in visit",
      System = "http://hl7.org/fhir/v2/0276"
    };

    /// <summary>
    /// Literal for code: ARoutineCheckUpSuchAsAnAnnualPhysical
    /// </summary>
    public const string LiteralARoutineCheckUpSuchAsAnAnnualPhysical = "CHECKUP";

    /// <summary>
    /// Literal for code: EmergencyAppointment
    /// </summary>
    public const string LiteralEmergencyAppointment = "EMERGENCY";

    /// <summary>
    /// Literal for code: AFollowUpVisitFromAPreviousAppointment
    /// </summary>
    public const string LiteralAFollowUpVisitFromAPreviousAppointment = "FOLLOWUP";

    /// <summary>
    /// Literal for code: RoutineAppointmentDefaultIfNotValued
    /// </summary>
    public const string LiteralRoutineAppointmentDefaultIfNotValued = "ROUTINE";

    /// <summary>
    /// Literal for code: APreviouslyUnscheduledWalkInVisit
    /// </summary>
    public const string LiteralAPreviouslyUnscheduledWalkInVisit = "WALKIN";
  };
}
