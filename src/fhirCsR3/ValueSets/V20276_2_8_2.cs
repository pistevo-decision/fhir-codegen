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
    /// A routine check-up, such as an annual physical
    /// </summary>
    public static readonly Coding ARoutineCheckUpSuchAsAnAnnualPhysical = new Coding
    {
      Code = "CHECKUP",
      Display = "A routine check-up, such as an annual physical",
      System = "http://hl7.org/fhir/v2/0276"
    };
    /// <summary>
    /// Emergency appointment
    /// </summary>
    public static readonly Coding EmergencyAppointment = new Coding
    {
      Code = "EMERGENCY",
      Display = "Emergency appointment",
      System = "http://hl7.org/fhir/v2/0276"
    };
    /// <summary>
    /// A follow up visit from a previous appointment
    /// </summary>
    public static readonly Coding AFollowUpVisitFromAPreviousAppointment = new Coding
    {
      Code = "FOLLOWUP",
      Display = "A follow up visit from a previous appointment",
      System = "http://hl7.org/fhir/v2/0276"
    };
    /// <summary>
    /// Routine appointment - default if not valued
    /// </summary>
    public static readonly Coding RoutineAppointmentDefaultIfNotValued = new Coding
    {
      Code = "ROUTINE",
      Display = "Routine appointment - default if not valued",
      System = "http://hl7.org/fhir/v2/0276"
    };
    /// <summary>
    /// A previously unscheduled walk-in visit
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
    /// Literal for code: V20276ARoutineCheckUpSuchAsAnAnnualPhysical
    /// </summary>
    public const string LiteralV20276ARoutineCheckUpSuchAsAnAnnualPhysical = "http://hl7.org/fhir/v2/0276#CHECKUP";

    /// <summary>
    /// Literal for code: EmergencyAppointment
    /// </summary>
    public const string LiteralEmergencyAppointment = "EMERGENCY";

    /// <summary>
    /// Literal for code: V20276EmergencyAppointment
    /// </summary>
    public const string LiteralV20276EmergencyAppointment = "http://hl7.org/fhir/v2/0276#EMERGENCY";

    /// <summary>
    /// Literal for code: AFollowUpVisitFromAPreviousAppointment
    /// </summary>
    public const string LiteralAFollowUpVisitFromAPreviousAppointment = "FOLLOWUP";

    /// <summary>
    /// Literal for code: V20276AFollowUpVisitFromAPreviousAppointment
    /// </summary>
    public const string LiteralV20276AFollowUpVisitFromAPreviousAppointment = "http://hl7.org/fhir/v2/0276#FOLLOWUP";

    /// <summary>
    /// Literal for code: RoutineAppointmentDefaultIfNotValued
    /// </summary>
    public const string LiteralRoutineAppointmentDefaultIfNotValued = "ROUTINE";

    /// <summary>
    /// Literal for code: V20276RoutineAppointmentDefaultIfNotValued
    /// </summary>
    public const string LiteralV20276RoutineAppointmentDefaultIfNotValued = "http://hl7.org/fhir/v2/0276#ROUTINE";

    /// <summary>
    /// Literal for code: APreviouslyUnscheduledWalkInVisit
    /// </summary>
    public const string LiteralAPreviouslyUnscheduledWalkInVisit = "WALKIN";

    /// <summary>
    /// Literal for code: V20276APreviouslyUnscheduledWalkInVisit
    /// </summary>
    public const string LiteralV20276APreviouslyUnscheduledWalkInVisit = "http://hl7.org/fhir/v2/0276#WALKIN";

    /// <summary>
    /// Dictionary for looking up V20276 Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "CHECKUP", ARoutineCheckUpSuchAsAnAnnualPhysical }, 
      { "http://hl7.org/fhir/v2/0276#CHECKUP", ARoutineCheckUpSuchAsAnAnnualPhysical }, 
      { "EMERGENCY", EmergencyAppointment }, 
      { "http://hl7.org/fhir/v2/0276#EMERGENCY", EmergencyAppointment }, 
      { "FOLLOWUP", AFollowUpVisitFromAPreviousAppointment }, 
      { "http://hl7.org/fhir/v2/0276#FOLLOWUP", AFollowUpVisitFromAPreviousAppointment }, 
      { "ROUTINE", RoutineAppointmentDefaultIfNotValued }, 
      { "http://hl7.org/fhir/v2/0276#ROUTINE", RoutineAppointmentDefaultIfNotValued }, 
      { "WALKIN", APreviouslyUnscheduledWalkInVisit }, 
      { "http://hl7.org/fhir/v2/0276#WALKIN", APreviouslyUnscheduledWalkInVisit }, 
    };
  };
}