// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// This value set lists all the event or request resource types defined in this version of the specification.
  /// </summary>
  public static class EventOrRequestResourceTypesCodes
  {
    /// <summary>
    /// A booking of a healthcare event among patient(s), practitioner(s), related person(s) and/or device(s) for a specific date/time. This may result in one or more Encounter(s).
    /// </summary>
    public static readonly Coding Appointment_request_resource_types = new Coding
    {
      Code = "Appointment",
      Display = "Appointment",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// A reply to an appointment request for a patient and/or practitioner(s), such as a confirmation or rejection.
    /// </summary>
    public static readonly Coding AppointmentResponse_request_resource_types = new Coding
    {
      Code = "AppointmentResponse",
      Display = "AppointmentResponse",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// Healthcare plan for patient or group.
    /// </summary>
    public static readonly Coding CarePlan_request_resource_types = new Coding
    {
      Code = "CarePlan",
      Display = "CarePlan",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// Item containing charge code(s) associated with the provision of healthcare provider products.
    /// </summary>
    public static readonly Coding ChargeItem_event_resource_types = new Coding
    {
      Code = "ChargeItem",
      Display = "ChargeItem",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Claim, Pre-determination or Pre-authorization.
    /// </summary>
    public static readonly Coding Claim_request_resource_types = new Coding
    {
      Code = "Claim",
      Display = "Claim",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// Remittance resource.
    /// </summary>
    public static readonly Coding ClaimResponse_event_resource_types = new Coding
    {
      Code = "ClaimResponse",
      Display = "ClaimResponse",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A clinical assessment performed when planning treatments and management strategies for a patient.
    /// </summary>
    public static readonly Coding ClinicalImpression_event_resource_types = new Coding
    {
      Code = "ClinicalImpression",
      Display = "ClinicalImpression",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A record of information transmitted from a sender to a receiver.
    /// </summary>
    public static readonly Coding Communication_event_resource_types = new Coding
    {
      Code = "Communication",
      Display = "Communication",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A request for information to be sent to a receiver.
    /// </summary>
    public static readonly Coding CommunicationRequest_request_resource_types = new Coding
    {
      Code = "CommunicationRequest",
      Display = "CommunicationRequest",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// A set of resources composed into a single coherent clinical statement with clinical attestation.
    /// </summary>
    public static readonly Coding Composition_event_resource_types = new Coding
    {
      Code = "Composition",
      Display = "Composition",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Detailed information about conditions, problems or diagnoses.
    /// </summary>
    public static readonly Coding Condition_event_resource_types = new Coding
    {
      Code = "Condition",
      Display = "Condition",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A healthcare consumer's policy choices to permits or denies recipients or roles to perform actions for specific purposes and periods of time.
    /// </summary>
    public static readonly Coding Consent_event_resource_types = new Coding
    {
      Code = "Consent",
      Display = "Consent",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Legal Agreement.
    /// </summary>
    public static readonly Coding Contract_request_resource_types = new Coding
    {
      Code = "Contract",
      Display = "Contract",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// Insurance or medical plan or a payment agreement.
    /// </summary>
    public static readonly Coding Coverage_event_resource_types = new Coding
    {
      Code = "Coverage",
      Display = "Coverage",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Medical device request.
    /// </summary>
    public static readonly Coding DeviceRequest_request_resource_types = new Coding
    {
      Code = "DeviceRequest",
      Display = "DeviceRequest",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// Record of use of a device.
    /// </summary>
    public static readonly Coding DeviceUsage_event_resource_types = new Coding
    {
      Code = "DeviceUsage",
      Display = "DeviceUsage",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A Diagnostic report - a combination of request information, atomic results, images, interpretation, as well as formatted reports.
    /// </summary>
    public static readonly Coding DiagnosticReport_event_resource_types = new Coding
    {
      Code = "DiagnosticReport",
      Display = "DiagnosticReport",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A list that defines a set of documents.
    /// </summary>
    public static readonly Coding DocumentManifest_event_resource_types = new Coding
    {
      Code = "DocumentManifest",
      Display = "DocumentManifest",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A reference to a document.
    /// </summary>
    public static readonly Coding DocumentReference_event_resource_types = new Coding
    {
      Code = "DocumentReference",
      Display = "DocumentReference",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// An interaction during which services are provided to the patient.
    /// </summary>
    public static readonly Coding Encounter_event_resource_types = new Coding
    {
      Code = "Encounter",
      Display = "Encounter",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Enrollment request.
    /// </summary>
    public static readonly Coding EnrollmentRequest_request_resource_types = new Coding
    {
      Code = "EnrollmentRequest",
      Display = "EnrollmentRequest",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// EnrollmentResponse resource.
    /// </summary>
    public static readonly Coding EnrollmentResponse_event_resource_types = new Coding
    {
      Code = "EnrollmentResponse",
      Display = "EnrollmentResponse",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// An association of a Patient with an Organization and  Healthcare Provider(s) for a period of time that the Organization assumes some level of responsibility.
    /// </summary>
    public static readonly Coding EpisodeOfCare_event_resource_types = new Coding
    {
      Code = "EpisodeOfCare",
      Display = "EpisodeOfCare",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Explanation of Benefit resource.
    /// </summary>
    public static readonly Coding ExplanationOfBenefit_event_resource_types = new Coding
    {
      Code = "ExplanationOfBenefit",
      Display = "ExplanationOfBenefit",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Information about patient's relatives, relevant for patient.
    /// </summary>
    public static readonly Coding FamilyMemberHistory_event_resource_types = new Coding
    {
      Code = "FamilyMemberHistory",
      Display = "FamilyMemberHistory",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// The formal response to a guidance request.
    /// </summary>
    public static readonly Coding GuidanceResponse_event_resource_types = new Coding
    {
      Code = "GuidanceResponse",
      Display = "GuidanceResponse",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A set of images produced in single study (one or more series of references images).
    /// </summary>
    public static readonly Coding ImagingStudy_event_resource_types = new Coding
    {
      Code = "ImagingStudy",
      Display = "ImagingStudy",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Immunization event information.
    /// </summary>
    public static readonly Coding Immunization_event_resource_types = new Coding
    {
      Code = "Immunization",
      Display = "Immunization",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Guidance or advice relating to an immunization.
    /// </summary>
    public static readonly Coding ImmunizationRecommendation_request_resource_types = new Coding
    {
      Code = "ImmunizationRecommendation",
      Display = "ImmunizationRecommendation",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// Results of a measure evaluation.
    /// </summary>
    public static readonly Coding MeasureReport_event_resource_types = new Coding
    {
      Code = "MeasureReport",
      Display = "MeasureReport",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A photo, video, or audio recording acquired or used in healthcare. The actual content may be inline or provided by direct reference.
    /// </summary>
    public static readonly Coding Media_event_resource_types = new Coding
    {
      Code = "Media",
      Display = "Media",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Administration of medication to a patient.
    /// </summary>
    public static readonly Coding MedicationAdministration_event_resource_types = new Coding
    {
      Code = "MedicationAdministration",
      Display = "MedicationAdministration",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Dispensing a medication to a named patient.
    /// </summary>
    public static readonly Coding MedicationDispense_event_resource_types = new Coding
    {
      Code = "MedicationDispense",
      Display = "MedicationDispense",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Ordering of medication for patient or group.
    /// </summary>
    public static readonly Coding MedicationRequest_request_resource_types = new Coding
    {
      Code = "MedicationRequest",
      Display = "MedicationRequest",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// Record of medication being taken by a patient.
    /// </summary>
    public static readonly Coding MedicationUsage_event_resource_types = new Coding
    {
      Code = "MedicationUsage",
      Display = "MedicationUsage",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Diet, formula or nutritional supplement request.
    /// </summary>
    public static readonly Coding NutritionOrder_request_resource_types = new Coding
    {
      Code = "NutritionOrder",
      Display = "NutritionOrder",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// Measurements and simple assertions.
    /// </summary>
    public static readonly Coding Observation_event_resource_types = new Coding
    {
      Code = "Observation",
      Display = "Observation",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// PaymentNotice request.
    /// </summary>
    public static readonly Coding PaymentNotice_event_resource_types = new Coding
    {
      Code = "PaymentNotice",
      Display = "PaymentNotice",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// PaymentReconciliation resource.
    /// </summary>
    public static readonly Coding PaymentReconciliation_event_resource_types = new Coding
    {
      Code = "PaymentReconciliation",
      Display = "PaymentReconciliation",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// An action that is being or was performed on a patient.
    /// </summary>
    public static readonly Coding Procedure_event_resource_types = new Coding
    {
      Code = "Procedure",
      Display = "Procedure",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// ProcessResponse resource.
    /// </summary>
    public static readonly Coding ProcessResponse_event_resource_types = new Coding
    {
      Code = "ProcessResponse",
      Display = "ProcessResponse",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A structured set of questions and their answers.
    /// </summary>
    public static readonly Coding QuestionnaireResponse_event_resource_types = new Coding
    {
      Code = "QuestionnaireResponse",
      Display = "QuestionnaireResponse",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Potential outcomes for a subject with likelihood.
    /// </summary>
    public static readonly Coding RiskAssessment_event_resource_types = new Coding
    {
      Code = "RiskAssessment",
      Display = "RiskAssessment",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A record of a request for service such as diagnostic investigations, treatments, or operations to be performed.
    /// </summary>
    public static readonly Coding ServiceRequest_request_resource_types = new Coding
    {
      Code = "ServiceRequest",
      Display = "ServiceRequest",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// Delivery of bulk Supplies.
    /// </summary>
    public static readonly Coding SupplyDelivery_event_resource_types = new Coding
    {
      Code = "SupplyDelivery",
      Display = "SupplyDelivery",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// Request for a medication, substance or device.
    /// </summary>
    public static readonly Coding SupplyRequest_request_resource_types = new Coding
    {
      Code = "SupplyRequest",
      Display = "SupplyRequest",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// A task to be performed.
    /// </summary>
    public static readonly Coding Task_event_resource_types = new Coding
    {
      Code = "Task",
      Display = "Task",
      System = "http://hl7.org/fhir/event-resource-types"
    };
    /// <summary>
    /// A task to be performed.
    /// </summary>
    public static readonly Coding Task_request_resource_types = new Coding
    {
      Code = "Task",
      Display = "Task",
      System = "http://hl7.org/fhir/request-resource-types"
    };
    /// <summary>
    /// Prescription for vision correction products for a patient.
    /// </summary>
    public static readonly Coding VisionPrescription_request_resource_types = new Coding
    {
      Code = "VisionPrescription",
      Display = "VisionPrescription",
      System = "http://hl7.org/fhir/request-resource-types"
    };

    /// <summary>
    /// Literal for code: Appointment_request_resource_types
    /// </summary>
    public const string LiteralAppointment_request_resource_types = "Appointment";

    /// <summary>
    /// Literal for code: AppointmentResponse_request_resource_types
    /// </summary>
    public const string LiteralAppointmentResponse_request_resource_types = "AppointmentResponse";

    /// <summary>
    /// Literal for code: CarePlan_request_resource_types
    /// </summary>
    public const string LiteralCarePlan_request_resource_types = "CarePlan";

    /// <summary>
    /// Literal for code: ChargeItem_event_resource_types
    /// </summary>
    public const string LiteralChargeItem_event_resource_types = "ChargeItem";

    /// <summary>
    /// Literal for code: Claim_request_resource_types
    /// </summary>
    public const string LiteralClaim_request_resource_types = "Claim";

    /// <summary>
    /// Literal for code: ClaimResponse_event_resource_types
    /// </summary>
    public const string LiteralClaimResponse_event_resource_types = "ClaimResponse";

    /// <summary>
    /// Literal for code: ClinicalImpression_event_resource_types
    /// </summary>
    public const string LiteralClinicalImpression_event_resource_types = "ClinicalImpression";

    /// <summary>
    /// Literal for code: Communication_event_resource_types
    /// </summary>
    public const string LiteralCommunication_event_resource_types = "Communication";

    /// <summary>
    /// Literal for code: CommunicationRequest_request_resource_types
    /// </summary>
    public const string LiteralCommunicationRequest_request_resource_types = "CommunicationRequest";

    /// <summary>
    /// Literal for code: Composition_event_resource_types
    /// </summary>
    public const string LiteralComposition_event_resource_types = "Composition";

    /// <summary>
    /// Literal for code: Condition_event_resource_types
    /// </summary>
    public const string LiteralCondition_event_resource_types = "Condition";

    /// <summary>
    /// Literal for code: Consent_event_resource_types
    /// </summary>
    public const string LiteralConsent_event_resource_types = "Consent";

    /// <summary>
    /// Literal for code: Contract_request_resource_types
    /// </summary>
    public const string LiteralContract_request_resource_types = "Contract";

    /// <summary>
    /// Literal for code: Coverage_event_resource_types
    /// </summary>
    public const string LiteralCoverage_event_resource_types = "Coverage";

    /// <summary>
    /// Literal for code: DeviceRequest_request_resource_types
    /// </summary>
    public const string LiteralDeviceRequest_request_resource_types = "DeviceRequest";

    /// <summary>
    /// Literal for code: DeviceUsage_event_resource_types
    /// </summary>
    public const string LiteralDeviceUsage_event_resource_types = "DeviceUsage";

    /// <summary>
    /// Literal for code: DiagnosticReport_event_resource_types
    /// </summary>
    public const string LiteralDiagnosticReport_event_resource_types = "DiagnosticReport";

    /// <summary>
    /// Literal for code: DocumentManifest_event_resource_types
    /// </summary>
    public const string LiteralDocumentManifest_event_resource_types = "DocumentManifest";

    /// <summary>
    /// Literal for code: DocumentReference_event_resource_types
    /// </summary>
    public const string LiteralDocumentReference_event_resource_types = "DocumentReference";

    /// <summary>
    /// Literal for code: Encounter_event_resource_types
    /// </summary>
    public const string LiteralEncounter_event_resource_types = "Encounter";

    /// <summary>
    /// Literal for code: EnrollmentRequest_request_resource_types
    /// </summary>
    public const string LiteralEnrollmentRequest_request_resource_types = "EnrollmentRequest";

    /// <summary>
    /// Literal for code: EnrollmentResponse_event_resource_types
    /// </summary>
    public const string LiteralEnrollmentResponse_event_resource_types = "EnrollmentResponse";

    /// <summary>
    /// Literal for code: EpisodeOfCare_event_resource_types
    /// </summary>
    public const string LiteralEpisodeOfCare_event_resource_types = "EpisodeOfCare";

    /// <summary>
    /// Literal for code: ExplanationOfBenefit_event_resource_types
    /// </summary>
    public const string LiteralExplanationOfBenefit_event_resource_types = "ExplanationOfBenefit";

    /// <summary>
    /// Literal for code: FamilyMemberHistory_event_resource_types
    /// </summary>
    public const string LiteralFamilyMemberHistory_event_resource_types = "FamilyMemberHistory";

    /// <summary>
    /// Literal for code: GuidanceResponse_event_resource_types
    /// </summary>
    public const string LiteralGuidanceResponse_event_resource_types = "GuidanceResponse";

    /// <summary>
    /// Literal for code: ImagingStudy_event_resource_types
    /// </summary>
    public const string LiteralImagingStudy_event_resource_types = "ImagingStudy";

    /// <summary>
    /// Literal for code: Immunization_event_resource_types
    /// </summary>
    public const string LiteralImmunization_event_resource_types = "Immunization";

    /// <summary>
    /// Literal for code: ImmunizationRecommendation_request_resource_types
    /// </summary>
    public const string LiteralImmunizationRecommendation_request_resource_types = "ImmunizationRecommendation";

    /// <summary>
    /// Literal for code: MeasureReport_event_resource_types
    /// </summary>
    public const string LiteralMeasureReport_event_resource_types = "MeasureReport";

    /// <summary>
    /// Literal for code: Media_event_resource_types
    /// </summary>
    public const string LiteralMedia_event_resource_types = "Media";

    /// <summary>
    /// Literal for code: MedicationAdministration_event_resource_types
    /// </summary>
    public const string LiteralMedicationAdministration_event_resource_types = "MedicationAdministration";

    /// <summary>
    /// Literal for code: MedicationDispense_event_resource_types
    /// </summary>
    public const string LiteralMedicationDispense_event_resource_types = "MedicationDispense";

    /// <summary>
    /// Literal for code: MedicationRequest_request_resource_types
    /// </summary>
    public const string LiteralMedicationRequest_request_resource_types = "MedicationRequest";

    /// <summary>
    /// Literal for code: MedicationUsage_event_resource_types
    /// </summary>
    public const string LiteralMedicationUsage_event_resource_types = "MedicationUsage";

    /// <summary>
    /// Literal for code: NutritionOrder_request_resource_types
    /// </summary>
    public const string LiteralNutritionOrder_request_resource_types = "NutritionOrder";

    /// <summary>
    /// Literal for code: Observation_event_resource_types
    /// </summary>
    public const string LiteralObservation_event_resource_types = "Observation";

    /// <summary>
    /// Literal for code: PaymentNotice_event_resource_types
    /// </summary>
    public const string LiteralPaymentNotice_event_resource_types = "PaymentNotice";

    /// <summary>
    /// Literal for code: PaymentReconciliation_event_resource_types
    /// </summary>
    public const string LiteralPaymentReconciliation_event_resource_types = "PaymentReconciliation";

    /// <summary>
    /// Literal for code: Procedure_event_resource_types
    /// </summary>
    public const string LiteralProcedure_event_resource_types = "Procedure";

    /// <summary>
    /// Literal for code: ProcessResponse_event_resource_types
    /// </summary>
    public const string LiteralProcessResponse_event_resource_types = "ProcessResponse";

    /// <summary>
    /// Literal for code: QuestionnaireResponse_event_resource_types
    /// </summary>
    public const string LiteralQuestionnaireResponse_event_resource_types = "QuestionnaireResponse";

    /// <summary>
    /// Literal for code: RiskAssessment_event_resource_types
    /// </summary>
    public const string LiteralRiskAssessment_event_resource_types = "RiskAssessment";

    /// <summary>
    /// Literal for code: ServiceRequest_request_resource_types
    /// </summary>
    public const string LiteralServiceRequest_request_resource_types = "ServiceRequest";

    /// <summary>
    /// Literal for code: SupplyDelivery_event_resource_types
    /// </summary>
    public const string LiteralSupplyDelivery_event_resource_types = "SupplyDelivery";

    /// <summary>
    /// Literal for code: SupplyRequest_request_resource_types
    /// </summary>
    public const string LiteralSupplyRequest_request_resource_types = "SupplyRequest";

    /// <summary>
    /// Literal for code: Task_event_resource_types
    /// </summary>
    public const string LiteralTask_event_resource_types = "Task";

    /// <summary>
    /// Literal for code: Task_request_resource_types
    /// </summary>
    public const string LiteralTask_request_resource_types = "Task";

    /// <summary>
    /// Literal for code: VisionPrescription_request_resource_types
    /// </summary>
    public const string LiteralVisionPrescription_request_resource_types = "VisionPrescription";
  };
}
