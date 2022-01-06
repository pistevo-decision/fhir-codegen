// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Event Types for Audit Events - defined by DICOM with some FHIR specific additions.
  /// </summary>
  public static class AuditEventTypeCodes
  {
    /// <summary>
    /// Audit event: Application Activity has taken place
    /// </summary>
    public static readonly Coding ApplicationActivity_dicom_dcim = new Coding
    {
      Code = "110100",
      Display = "Application Activity",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Audit Log has been used
    /// </summary>
    public static readonly Coding AuditLogUsed_dicom_dcim = new Coding
    {
      Code = "110101",
      Display = "Audit Log Used",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Storage of DICOM Instances has begun
    /// </summary>
    public static readonly Coding BeginTransferringDICOMInstances_dicom_dcim = new Coding
    {
      Code = "110102",
      Display = "Begin Transferring DICOM Instances",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: DICOM Instances have been created, read, updated, or deleted
    /// </summary>
    public static readonly Coding DICOMInstancesAccessed_dicom_dcim = new Coding
    {
      Code = "110103",
      Display = "DICOM Instances Accessed",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Storage of DICOM Instances has been completed
    /// </summary>
    public static readonly Coding DICOMInstancesTransferred_dicom_dcim = new Coding
    {
      Code = "110104",
      Display = "DICOM Instances Transferred",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Entire Study has been deleted
    /// </summary>
    public static readonly Coding DICOMStudyDeleted_dicom_dcim = new Coding
    {
      Code = "110105",
      Display = "DICOM Study Deleted",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Data has been exported out of the system
    /// </summary>
    public static readonly Coding Export_dicom_dcim = new Coding
    {
      Code = "110106",
      Display = "Export",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Data has been imported into the system
    /// </summary>
    public static readonly Coding Import_dicom_dcim = new Coding
    {
      Code = "110107",
      Display = "Import",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: System has joined or left network
    /// </summary>
    public static readonly Coding NetworkEntry_dicom_dcim = new Coding
    {
      Code = "110108",
      Display = "Network Entry",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Order has been created, read, updated or deleted
    /// </summary>
    public static readonly Coding OrderRecord_dicom_dcim = new Coding
    {
      Code = "110109",
      Display = "Order Record",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Patient Record has been created, read, updated, or deleted
    /// </summary>
    public static readonly Coding PatientRecord_dicom_dcim = new Coding
    {
      Code = "110110",
      Display = "Patient Record",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Procedure Record has been created, read, updated, or deleted
    /// </summary>
    public static readonly Coding ProcedureRecord_dicom_dcim = new Coding
    {
      Code = "110111",
      Display = "Procedure Record",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Query has been made
    /// </summary>
    public static readonly Coding Query_dicom_dcim = new Coding
    {
      Code = "110112",
      Display = "Query",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: Security Alert has been raised
    /// </summary>
    public static readonly Coding SecurityAlert_dicom_dcim = new Coding
    {
      Code = "110113",
      Display = "Security Alert",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit event: User Authentication has been attempted
    /// </summary>
    public static readonly Coding UserAuthentication_dicom_dcim = new Coding
    {
      Code = "110114",
      Display = "User Authentication",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Audit Event: Execution of a RESTful operation as defined by FHIR.
    /// </summary>
    public static readonly Coding RESTfulOperation_audit_event_type = new Coding
    {
      Code = "rest",
      Display = "RESTful Operation",
      System = "http://hl7.org/fhir/audit-event-type"
    };

    /// <summary>
    /// Literal for code: ApplicationActivity_dicom_dcim
    /// </summary>
    public const string LiteralApplicationActivity_dicom_dcim = "110100";

    /// <summary>
    /// Literal for code: AuditLogUsed_dicom_dcim
    /// </summary>
    public const string LiteralAuditLogUsed_dicom_dcim = "110101";

    /// <summary>
    /// Literal for code: BeginTransferringDICOMInstances_dicom_dcim
    /// </summary>
    public const string LiteralBeginTransferringDICOMInstances_dicom_dcim = "110102";

    /// <summary>
    /// Literal for code: DICOMInstancesAccessed_dicom_dcim
    /// </summary>
    public const string LiteralDICOMInstancesAccessed_dicom_dcim = "110103";

    /// <summary>
    /// Literal for code: DICOMInstancesTransferred_dicom_dcim
    /// </summary>
    public const string LiteralDICOMInstancesTransferred_dicom_dcim = "110104";

    /// <summary>
    /// Literal for code: DICOMStudyDeleted_dicom_dcim
    /// </summary>
    public const string LiteralDICOMStudyDeleted_dicom_dcim = "110105";

    /// <summary>
    /// Literal for code: Export_dicom_dcim
    /// </summary>
    public const string LiteralExport_dicom_dcim = "110106";

    /// <summary>
    /// Literal for code: Import_dicom_dcim
    /// </summary>
    public const string LiteralImport_dicom_dcim = "110107";

    /// <summary>
    /// Literal for code: NetworkEntry_dicom_dcim
    /// </summary>
    public const string LiteralNetworkEntry_dicom_dcim = "110108";

    /// <summary>
    /// Literal for code: OrderRecord_dicom_dcim
    /// </summary>
    public const string LiteralOrderRecord_dicom_dcim = "110109";

    /// <summary>
    /// Literal for code: PatientRecord_dicom_dcim
    /// </summary>
    public const string LiteralPatientRecord_dicom_dcim = "110110";

    /// <summary>
    /// Literal for code: ProcedureRecord_dicom_dcim
    /// </summary>
    public const string LiteralProcedureRecord_dicom_dcim = "110111";

    /// <summary>
    /// Literal for code: Query_dicom_dcim
    /// </summary>
    public const string LiteralQuery_dicom_dcim = "110112";

    /// <summary>
    /// Literal for code: SecurityAlert_dicom_dcim
    /// </summary>
    public const string LiteralSecurityAlert_dicom_dcim = "110113";

    /// <summary>
    /// Literal for code: UserAuthentication_dicom_dcim
    /// </summary>
    public const string LiteralUserAuthentication_dicom_dcim = "110114";

    /// <summary>
    /// Literal for code: RESTfulOperation_audit_event_type
    /// </summary>
    public const string LiteralRESTfulOperation_audit_event_type = "rest";
  };
}
