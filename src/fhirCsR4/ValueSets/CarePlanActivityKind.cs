// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Resource types defined as part of FHIR that can be represented as in-line definitions of a care plan activity.
  /// </summary>
  public static class CarePlanActivityKindCodes
  {
    /// <summary>
    /// A booking of a healthcare event among patient(s), practitioner(s), related person(s) and/or device(s) for a specific date/time. This may result in one or more Encounter(s).
    /// </summary>
    public static readonly Coding Appointment = new Coding
    {
      Code = "Appointment",
      Display = "Appointment",
      System = "http://hl7.org/fhir/resource-types"
    };
    /// <summary>
    /// A request to convey information; e.g. the CDS system proposes that an alert be sent to a responsible provider, the CDS system proposes that the public health agency be notified about a reportable condition.
    /// </summary>
    public static readonly Coding CommunicationRequest = new Coding
    {
      Code = "CommunicationRequest",
      Display = "CommunicationRequest",
      System = "http://hl7.org/fhir/resource-types"
    };
    /// <summary>
    /// Represents a request for a patient to employ a medical device. The device may be an implantable device, or an external assistive device, such as a walker.
    /// </summary>
    public static readonly Coding DeviceRequest = new Coding
    {
      Code = "DeviceRequest",
      Display = "DeviceRequest",
      System = "http://hl7.org/fhir/resource-types"
    };
    /// <summary>
    /// An order or request for both supply of the medication and the instructions for administration of the medication to a patient. The resource is called "MedicationRequest" rather than "MedicationPrescription" or "MedicationOrder" to generalize the use across inpatient and outpatient settings, including care plans, etc., and to harmonize with workflow patterns.
    /// </summary>
    public static readonly Coding MedicationRequest = new Coding
    {
      Code = "MedicationRequest",
      Display = "MedicationRequest",
      System = "http://hl7.org/fhir/resource-types"
    };
    /// <summary>
    /// A request to supply a diet, formula feeding (enteral) or oral nutritional supplement to a patient/resident.
    /// </summary>
    public static readonly Coding NutritionOrder = new Coding
    {
      Code = "NutritionOrder",
      Display = "NutritionOrder",
      System = "http://hl7.org/fhir/resource-types"
    };
    /// <summary>
    /// A record of a request for service such as diagnostic investigations, treatments, or operations to be performed.
    /// </summary>
    public static readonly Coding ServiceRequest = new Coding
    {
      Code = "ServiceRequest",
      Display = "ServiceRequest",
      System = "http://hl7.org/fhir/resource-types"
    };
    /// <summary>
    /// A task to be performed.
    /// </summary>
    public static readonly Coding Task = new Coding
    {
      Code = "Task",
      Display = "Task",
      System = "http://hl7.org/fhir/resource-types"
    };
    /// <summary>
    /// An authorization for the provision of glasses and/or contact lenses to a patient.
    /// </summary>
    public static readonly Coding VisionPrescription = new Coding
    {
      Code = "VisionPrescription",
      Display = "VisionPrescription",
      System = "http://hl7.org/fhir/resource-types"
    };

    /// <summary>
    /// Literal for code: Appointment
    /// </summary>
    public const string LiteralAppointment = "Appointment";

    /// <summary>
    /// Literal for code: CommunicationRequest
    /// </summary>
    public const string LiteralCommunicationRequest = "CommunicationRequest";

    /// <summary>
    /// Literal for code: DeviceRequest
    /// </summary>
    public const string LiteralDeviceRequest = "DeviceRequest";

    /// <summary>
    /// Literal for code: MedicationRequest
    /// </summary>
    public const string LiteralMedicationRequest = "MedicationRequest";

    /// <summary>
    /// Literal for code: NutritionOrder
    /// </summary>
    public const string LiteralNutritionOrder = "NutritionOrder";

    /// <summary>
    /// Literal for code: ServiceRequest
    /// </summary>
    public const string LiteralServiceRequest = "ServiceRequest";

    /// <summary>
    /// Literal for code: Task
    /// </summary>
    public const string LiteralTask = "Task";

    /// <summary>
    /// Literal for code: VisionPrescription
    /// </summary>
    public const string LiteralVisionPrescription = "VisionPrescription";
  };
}
