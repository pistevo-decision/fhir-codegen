// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// The value set is defined to be the set of format codes defined by the IHE Technical Framework, and also including additional format codes defined by the    HL7. The value set is listed in HITSP C80 Table 2-153 Format Code Value Set Definition,    with additions published later by IHE as published    at http://wiki.ihe.net/index.php?title=IHE_Format_Codes   and with additions published later by HL7 as published at https://confluence.hl7.org/display/SD/Format+Codes+for+IHE+XDS.   This is the code specifying the technical format of the document. Along with the typeCode,    it should provide sufficient information to allow any potential document consumer to know    if it will be able to process the document. The code shall be sufficiently specific to    ensure processing/display by identifying a document encoding, structure and template. The actual list of codes here is incomplete
  /// </summary>
  public static class FormatcodesCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding ForDocumentsFollowingCCDA11ConstraintsUsingANonStructuredBody = new Coding
    {
      Code = "urn:hl7-org:sdwg:ccda-nonXMLBody:1.1",
      Display = "For documents following C-CDA 1.1 constraints using a non structured body.",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding ForDocumentsFollowingCCDA21ConstraintsUsingANonStructuredBody = new Coding
    {
      Code = "urn:hl7-org:sdwg:ccda-nonXMLBody:2.1",
      Display = "For documents following C-CDA 2.1 constraints using a non structured body.",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding ForDocumentsFollowingCCDA11ConstraintsUsingAStructuredBody = new Coding
    {
      Code = "urn:hl7-org:sdwg:ccda-structuredBody:1.1",
      Display = "For documents following C-CDA 1.1 constraints using a structured body.",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding ForDocumentsFollowingCCDA21ConstraintsUsingAStructuredBody = new Coding
    {
      Code = "urn:hl7-org:sdwg:ccda-structuredBody:2.1",
      Display = "For documents following C-CDA 2.1 constraints using a structured body.",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding CardiologyCRC = new Coding
    {
      Code = "urn:ihe:card:CRC:2012",
      Display = "Cardiology CRC",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding CardiologyEPRCIE = new Coding
    {
      Code = "urn:ihe:card:EPRC-IE:2014",
      Display = "Cardiology EPRC-IE",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding CardiacImagingReport = new Coding
    {
      Code = "urn:ihe:card:imaging:2011",
      Display = "Cardiac Imaging Report",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding DentalCDA = new Coding
    {
      Code = "urn:ihe:dent:CDA:ImagingReportStructuredHeadings:2013",
      Display = "Dental CDA",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding DentalPDF = new Coding
    {
      Code = "urn:ihe:dent:PDF",
      Display = "Dental PDF",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding DentalText = new Coding
    {
      Code = "urn:ihe:dent:TEXT",
      Display = "Dental Text",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AdvancedPatientPrivacyConsents = new Coding
    {
      Code = "urn:ihe:iti:appc:2016:consent",
      Display = "Advanced Patient Privacy Consents",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding BasicPatientPrivacyConsentsWithScannedDocument = new Coding
    {
      Code = "urn:ihe:iti:bppc-sd:2007",
      Display = "Basic Patient Privacy Consents with Scanned Document",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding BasicPatientPrivacyConsents = new Coding
    {
      Code = "urn:ihe:iti:bppc:2007",
      Display = "Basic Patient Privacy Consents",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding DSGDetachedDocument = new Coding
    {
      Code = "urn:ihe:iti:dsg:detached:2014",
      Display = "DSG Detached Document",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding DSGEnvelopingDocument = new Coding
    {
      Code = "urn:ihe:iti:dsg:enveloping:2014",
      Display = "DSG Enveloping Document",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PDFEmbeddedInCDAPerXDSSDProfile = new Coding
    {
      Code = "urn:ihe:iti:xds-sd:pdf:2008",
      Display = "PDF embedded in CDA per XDS-SD profile",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding TextEmbeddedInCDAPerXDSSDProfile = new Coding
    {
      Code = "urn:ihe:iti:xds-sd:text:2008",
      Display = "Text embedded in CDA per XDS-SD profile",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MimeTypeSufficient = new Coding
    {
      Code = "urn:ihe:iti:xds:2017:mimeTypeSufficient",
      Display = "mimeType Sufficient",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding XDWWorkflowDocument = new Coding
    {
      Code = "urn:ihe:iti:xdw:2011:workflowDoc",
      Display = "XDW Workflow Document",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding CDALaboratoryReport = new Coding
    {
      Code = "urn:ihe:lab:xd-lab:2008",
      Display = "CDA Laboratory Report",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportAll = new Coding
    {
      Code = "urn:ihe:pat:apsr:all:2010",
      Display = "Anatomic Pathology Structured Report All",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerAll = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:all:2010",
      Display = "Anatomic Pathology Structured Report Cancer All",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerBreast = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:breast:2010",
      Display = "Anatomic Pathology Structured Report Cancer Breast",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerCervix = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:cervix:2010",
      Display = "Anatomic Pathology Structured Report Cancer Cervix",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerColon = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:colon:2010",
      Display = "Anatomic Pathology Structured Report Cancer Colon",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerEndometrium = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:endometrium:2010",
      Display = "Anatomic Pathology Structured Report Cancer Endometrium",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerEsophagus = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:esophagus:2010",
      Display = "Anatomic Pathology Structured Report Cancer Esophagus",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerKidney = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:kidney:2010",
      Display = "Anatomic Pathology Structured Report Cancer Kidney",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerLarynx = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:larynx:2010",
      Display = "Anatomic Pathology Structured Report Cancer Larynx",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerLipOralCavity = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:lip_oral_cavity:2010",
      Display = "Anatomic Pathology Structured Report Cancer Lip Oral Cavity",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerLiver = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:liver:2010",
      Display = "Anatomic Pathology Structured Report Cancer Liver",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerLung = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:lung:2010",
      Display = "Anatomic Pathology Structured Report Cancer Lung",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerOvary = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:ovary:2010",
      Display = "Anatomic Pathology Structured Report Cancer Ovary",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerPancreas = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:pancreas:2010",
      Display = "Anatomic Pathology Structured Report Cancer Pancreas",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerPharynx = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:pharynx:2010",
      Display = "Anatomic Pathology Structured Report Cancer Pharynx",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerProstate = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:prostate:2010",
      Display = "Anatomic Pathology Structured Report Cancer Prostate",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerSalivaryGland = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:salivary_gland:2010",
      Display = "Anatomic Pathology Structured Report Cancer Salivary Gland",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerSkin = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:skin:2010",
      Display = "Anatomic Pathology Structured Report Cancer Skin",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerStomach = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:stomach:2010",
      Display = "Anatomic Pathology Structured Report Cancer Stomach",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerTestis = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:testis:2010",
      Display = "Anatomic Pathology Structured Report Cancer Testis",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerThyroid = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:thyroid:2010",
      Display = "Anatomic Pathology Structured Report Cancer Thyroid",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AnatomicPathologyStructuredReportCancerUrinaryBladder = new Coding
    {
      Code = "urn:ihe:pat:apsr:cancer:urinary_bladder:2010",
      Display = "Anatomic Pathology Structured Report Cancer Urinary Bladder",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AntepartumRecordAPREducation = new Coding
    {
      Code = "urn:ihe:pcc:apr:edu:2008",
      Display = "Antepartum Record (APR) - Education",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AntepartumRecordAPRHistoryAndPhysical = new Coding
    {
      Code = "urn:ihe:pcc:apr:handp:2008",
      Display = "Antepartum Record (APR) - History and Physical",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding AntepartumRecordAPRLaboratory = new Coding
    {
      Code = "urn:ihe:pcc:apr:lab:2008",
      Display = "Antepartum Record (APR) - Laboratory",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding IHEAntepartumSummary = new Coding
    {
      Code = "urn:ihe:pcc:aps:2007",
      Display = "IHE Antepartum Summary",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding CareManagementCM = new Coding
    {
      Code = "urn:ihe:pcc:cm:2008",
      Display = "Care Management (CM)",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding CancerRegistryContentCRC = new Coding
    {
      Code = "urn:ihe:pcc:crc:2008",
      Display = "Cancer Registry Content (CRC)",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCCTN = new Coding
    {
      Code = "urn:ihe:pcc:ctn:2007",
      Display = "PCC CTN",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding EmergencyDepartmentEncounterSummaryEDES = new Coding
    {
      Code = "urn:ihe:pcc:edes:2007",
      Display = "Emergency Department Encounter Summary (EDES)",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCEDPN = new Coding
    {
      Code = "urn:ihe:pcc:edpn:2007",
      Display = "PCC EDPN",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding EmergencyDepartmentReferralEDR = new Coding
    {
      Code = "urn:ihe:pcc:edr:2007",
      Display = "Emergency Department Referral (EDR)",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCETS = new Coding
    {
      Code = "urn:ihe:pcc:ets:2011",
      Display = "PCC ETS",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCHP = new Coding
    {
      Code = "urn:ihe:pcc:hp:2008",
      Display = "PCC HP",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding ImmunizationContentIC = new Coding
    {
      Code = "urn:ihe:pcc:ic:2008",
      Display = "Immunization Content (IC)",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCITS = new Coding
    {
      Code = "urn:ihe:pcc:its:2011",
      Display = "PCC ITS",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCLDHP = new Coding
    {
      Code = "urn:ihe:pcc:ldhp:2009",
      Display = "PCC LDHP",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCLDS = new Coding
    {
      Code = "urn:ihe:pcc:lds:2009",
      Display = "PCC LDS",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCMDS = new Coding
    {
      Code = "urn:ihe:pcc:mds:2009",
      Display = "PCC MDS",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCNDS = new Coding
    {
      Code = "urn:ihe:pcc:nds:2010",
      Display = "PCC NDS",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCNN = new Coding
    {
      Code = "urn:ihe:pcc:nn:2007",
      Display = "PCC NN",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCPPVS = new Coding
    {
      Code = "urn:ihe:pcc:ppvs:2010",
      Display = "PCC PPVS",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding RoutineInterfacilityPatientTransportRIPT = new Coding
    {
      Code = "urn:ihe:pcc:ript:2017",
      Display = "Routine Interfacility Patient Transport (RIPT)",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCTN = new Coding
    {
      Code = "urn:ihe:pcc:tn:2007",
      Display = "PCC TN",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PCCTRS = new Coding
    {
      Code = "urn:ihe:pcc:trs:2011",
      Display = "PCC TRS",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding XDSMedicalSummaries = new Coding
    {
      Code = "urn:ihe:pcc:xds-ms:2007",
      Display = "XDS Medical Summaries",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PersonalHealthRecordsAlsoKnownAsHL7CCDAndHITSPC32 = new Coding
    {
      Code = "urn:ihe:pcc:xphr:2007",
      Display = "Personal Health Records. Also known as HL7 CCD and HITSP C32",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PharmacyDIS = new Coding
    {
      Code = "urn:ihe:pharm:dis:2010",
      Display = "Pharmacy DIS",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PharmacyPADV = new Coding
    {
      Code = "urn:ihe:pharm:padv:2010",
      Display = "Pharmacy PADV",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PharmacyPML = new Coding
    {
      Code = "urn:ihe:pharm:pml:2013",
      Display = "Pharmacy PML",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PharmacyPre = new Coding
    {
      Code = "urn:ihe:pharm:pre:2010",
      Display = "Pharmacy Pre",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding RadiologyXDSIStructuredCDA = new Coding
    {
      Code = "urn:ihe:rad:CDA:ImagingReportStructuredHeadings:2013",
      Display = "Radiology XDS-I Structured CDA",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding RadiologyXDSIPDF = new Coding
    {
      Code = "urn:ihe:rad:PDF",
      Display = "Radiology XDS-I PDF",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding RadiologyXDSIText = new Coding
    {
      Code = "urn:ihe:rad:TEXT",
      Display = "Radiology XDS-I Text",
      System = "http://ihe.net/fhir/ValueSet/IHE.FormatCode.codesystem"
    };

    /// <summary>
    /// Literal for code: ForDocumentsFollowingCCDA11ConstraintsUsingANonStructuredBody
    /// </summary>
    public const string LiteralForDocumentsFollowingCCDA11ConstraintsUsingANonStructuredBody = "urn:hl7-org:sdwg:ccda-nonXMLBody:1.1";

    /// <summary>
    /// Literal for code: ForDocumentsFollowingCCDA21ConstraintsUsingANonStructuredBody
    /// </summary>
    public const string LiteralForDocumentsFollowingCCDA21ConstraintsUsingANonStructuredBody = "urn:hl7-org:sdwg:ccda-nonXMLBody:2.1";

    /// <summary>
    /// Literal for code: ForDocumentsFollowingCCDA11ConstraintsUsingAStructuredBody
    /// </summary>
    public const string LiteralForDocumentsFollowingCCDA11ConstraintsUsingAStructuredBody = "urn:hl7-org:sdwg:ccda-structuredBody:1.1";

    /// <summary>
    /// Literal for code: ForDocumentsFollowingCCDA21ConstraintsUsingAStructuredBody
    /// </summary>
    public const string LiteralForDocumentsFollowingCCDA21ConstraintsUsingAStructuredBody = "urn:hl7-org:sdwg:ccda-structuredBody:2.1";

    /// <summary>
    /// Literal for code: CardiologyCRC
    /// </summary>
    public const string LiteralCardiologyCRC = "urn:ihe:card:CRC:2012";

    /// <summary>
    /// Literal for code: CardiologyEPRCIE
    /// </summary>
    public const string LiteralCardiologyEPRCIE = "urn:ihe:card:EPRC-IE:2014";

    /// <summary>
    /// Literal for code: CardiacImagingReport
    /// </summary>
    public const string LiteralCardiacImagingReport = "urn:ihe:card:imaging:2011";

    /// <summary>
    /// Literal for code: DentalCDA
    /// </summary>
    public const string LiteralDentalCDA = "urn:ihe:dent:CDA:ImagingReportStructuredHeadings:2013";

    /// <summary>
    /// Literal for code: DentalPDF
    /// </summary>
    public const string LiteralDentalPDF = "urn:ihe:dent:PDF";

    /// <summary>
    /// Literal for code: DentalText
    /// </summary>
    public const string LiteralDentalText = "urn:ihe:dent:TEXT";

    /// <summary>
    /// Literal for code: AdvancedPatientPrivacyConsents
    /// </summary>
    public const string LiteralAdvancedPatientPrivacyConsents = "urn:ihe:iti:appc:2016:consent";

    /// <summary>
    /// Literal for code: BasicPatientPrivacyConsentsWithScannedDocument
    /// </summary>
    public const string LiteralBasicPatientPrivacyConsentsWithScannedDocument = "urn:ihe:iti:bppc-sd:2007";

    /// <summary>
    /// Literal for code: BasicPatientPrivacyConsents
    /// </summary>
    public const string LiteralBasicPatientPrivacyConsents = "urn:ihe:iti:bppc:2007";

    /// <summary>
    /// Literal for code: DSGDetachedDocument
    /// </summary>
    public const string LiteralDSGDetachedDocument = "urn:ihe:iti:dsg:detached:2014";

    /// <summary>
    /// Literal for code: DSGEnvelopingDocument
    /// </summary>
    public const string LiteralDSGEnvelopingDocument = "urn:ihe:iti:dsg:enveloping:2014";

    /// <summary>
    /// Literal for code: PDFEmbeddedInCDAPerXDSSDProfile
    /// </summary>
    public const string LiteralPDFEmbeddedInCDAPerXDSSDProfile = "urn:ihe:iti:xds-sd:pdf:2008";

    /// <summary>
    /// Literal for code: TextEmbeddedInCDAPerXDSSDProfile
    /// </summary>
    public const string LiteralTextEmbeddedInCDAPerXDSSDProfile = "urn:ihe:iti:xds-sd:text:2008";

    /// <summary>
    /// Literal for code: MimeTypeSufficient
    /// </summary>
    public const string LiteralMimeTypeSufficient = "urn:ihe:iti:xds:2017:mimeTypeSufficient";

    /// <summary>
    /// Literal for code: XDWWorkflowDocument
    /// </summary>
    public const string LiteralXDWWorkflowDocument = "urn:ihe:iti:xdw:2011:workflowDoc";

    /// <summary>
    /// Literal for code: CDALaboratoryReport
    /// </summary>
    public const string LiteralCDALaboratoryReport = "urn:ihe:lab:xd-lab:2008";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportAll
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportAll = "urn:ihe:pat:apsr:all:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerAll
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerAll = "urn:ihe:pat:apsr:cancer:all:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerBreast
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerBreast = "urn:ihe:pat:apsr:cancer:breast:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerCervix
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerCervix = "urn:ihe:pat:apsr:cancer:cervix:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerColon
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerColon = "urn:ihe:pat:apsr:cancer:colon:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerEndometrium
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerEndometrium = "urn:ihe:pat:apsr:cancer:endometrium:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerEsophagus
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerEsophagus = "urn:ihe:pat:apsr:cancer:esophagus:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerKidney
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerKidney = "urn:ihe:pat:apsr:cancer:kidney:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerLarynx
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerLarynx = "urn:ihe:pat:apsr:cancer:larynx:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerLipOralCavity
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerLipOralCavity = "urn:ihe:pat:apsr:cancer:lip_oral_cavity:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerLiver
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerLiver = "urn:ihe:pat:apsr:cancer:liver:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerLung
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerLung = "urn:ihe:pat:apsr:cancer:lung:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerOvary
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerOvary = "urn:ihe:pat:apsr:cancer:ovary:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerPancreas
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerPancreas = "urn:ihe:pat:apsr:cancer:pancreas:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerPharynx
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerPharynx = "urn:ihe:pat:apsr:cancer:pharynx:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerProstate
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerProstate = "urn:ihe:pat:apsr:cancer:prostate:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerSalivaryGland
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerSalivaryGland = "urn:ihe:pat:apsr:cancer:salivary_gland:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerSkin
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerSkin = "urn:ihe:pat:apsr:cancer:skin:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerStomach
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerStomach = "urn:ihe:pat:apsr:cancer:stomach:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerTestis
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerTestis = "urn:ihe:pat:apsr:cancer:testis:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerThyroid
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerThyroid = "urn:ihe:pat:apsr:cancer:thyroid:2010";

    /// <summary>
    /// Literal for code: AnatomicPathologyStructuredReportCancerUrinaryBladder
    /// </summary>
    public const string LiteralAnatomicPathologyStructuredReportCancerUrinaryBladder = "urn:ihe:pat:apsr:cancer:urinary_bladder:2010";

    /// <summary>
    /// Literal for code: AntepartumRecordAPREducation
    /// </summary>
    public const string LiteralAntepartumRecordAPREducation = "urn:ihe:pcc:apr:edu:2008";

    /// <summary>
    /// Literal for code: AntepartumRecordAPRHistoryAndPhysical
    /// </summary>
    public const string LiteralAntepartumRecordAPRHistoryAndPhysical = "urn:ihe:pcc:apr:handp:2008";

    /// <summary>
    /// Literal for code: AntepartumRecordAPRLaboratory
    /// </summary>
    public const string LiteralAntepartumRecordAPRLaboratory = "urn:ihe:pcc:apr:lab:2008";

    /// <summary>
    /// Literal for code: IHEAntepartumSummary
    /// </summary>
    public const string LiteralIHEAntepartumSummary = "urn:ihe:pcc:aps:2007";

    /// <summary>
    /// Literal for code: CareManagementCM
    /// </summary>
    public const string LiteralCareManagementCM = "urn:ihe:pcc:cm:2008";

    /// <summary>
    /// Literal for code: CancerRegistryContentCRC
    /// </summary>
    public const string LiteralCancerRegistryContentCRC = "urn:ihe:pcc:crc:2008";

    /// <summary>
    /// Literal for code: PCCCTN
    /// </summary>
    public const string LiteralPCCCTN = "urn:ihe:pcc:ctn:2007";

    /// <summary>
    /// Literal for code: EmergencyDepartmentEncounterSummaryEDES
    /// </summary>
    public const string LiteralEmergencyDepartmentEncounterSummaryEDES = "urn:ihe:pcc:edes:2007";

    /// <summary>
    /// Literal for code: PCCEDPN
    /// </summary>
    public const string LiteralPCCEDPN = "urn:ihe:pcc:edpn:2007";

    /// <summary>
    /// Literal for code: EmergencyDepartmentReferralEDR
    /// </summary>
    public const string LiteralEmergencyDepartmentReferralEDR = "urn:ihe:pcc:edr:2007";

    /// <summary>
    /// Literal for code: PCCETS
    /// </summary>
    public const string LiteralPCCETS = "urn:ihe:pcc:ets:2011";

    /// <summary>
    /// Literal for code: PCCHP
    /// </summary>
    public const string LiteralPCCHP = "urn:ihe:pcc:hp:2008";

    /// <summary>
    /// Literal for code: ImmunizationContentIC
    /// </summary>
    public const string LiteralImmunizationContentIC = "urn:ihe:pcc:ic:2008";

    /// <summary>
    /// Literal for code: PCCITS
    /// </summary>
    public const string LiteralPCCITS = "urn:ihe:pcc:its:2011";

    /// <summary>
    /// Literal for code: PCCLDHP
    /// </summary>
    public const string LiteralPCCLDHP = "urn:ihe:pcc:ldhp:2009";

    /// <summary>
    /// Literal for code: PCCLDS
    /// </summary>
    public const string LiteralPCCLDS = "urn:ihe:pcc:lds:2009";

    /// <summary>
    /// Literal for code: PCCMDS
    /// </summary>
    public const string LiteralPCCMDS = "urn:ihe:pcc:mds:2009";

    /// <summary>
    /// Literal for code: PCCNDS
    /// </summary>
    public const string LiteralPCCNDS = "urn:ihe:pcc:nds:2010";

    /// <summary>
    /// Literal for code: PCCNN
    /// </summary>
    public const string LiteralPCCNN = "urn:ihe:pcc:nn:2007";

    /// <summary>
    /// Literal for code: PCCPPVS
    /// </summary>
    public const string LiteralPCCPPVS = "urn:ihe:pcc:ppvs:2010";

    /// <summary>
    /// Literal for code: RoutineInterfacilityPatientTransportRIPT
    /// </summary>
    public const string LiteralRoutineInterfacilityPatientTransportRIPT = "urn:ihe:pcc:ript:2017";

    /// <summary>
    /// Literal for code: PCCTN
    /// </summary>
    public const string LiteralPCCTN = "urn:ihe:pcc:tn:2007";

    /// <summary>
    /// Literal for code: PCCTRS
    /// </summary>
    public const string LiteralPCCTRS = "urn:ihe:pcc:trs:2011";

    /// <summary>
    /// Literal for code: XDSMedicalSummaries
    /// </summary>
    public const string LiteralXDSMedicalSummaries = "urn:ihe:pcc:xds-ms:2007";

    /// <summary>
    /// Literal for code: PersonalHealthRecordsAlsoKnownAsHL7CCDAndHITSPC32
    /// </summary>
    public const string LiteralPersonalHealthRecordsAlsoKnownAsHL7CCDAndHITSPC32 = "urn:ihe:pcc:xphr:2007";

    /// <summary>
    /// Literal for code: PharmacyDIS
    /// </summary>
    public const string LiteralPharmacyDIS = "urn:ihe:pharm:dis:2010";

    /// <summary>
    /// Literal for code: PharmacyPADV
    /// </summary>
    public const string LiteralPharmacyPADV = "urn:ihe:pharm:padv:2010";

    /// <summary>
    /// Literal for code: PharmacyPML
    /// </summary>
    public const string LiteralPharmacyPML = "urn:ihe:pharm:pml:2013";

    /// <summary>
    /// Literal for code: PharmacyPre
    /// </summary>
    public const string LiteralPharmacyPre = "urn:ihe:pharm:pre:2010";

    /// <summary>
    /// Literal for code: RadiologyXDSIStructuredCDA
    /// </summary>
    public const string LiteralRadiologyXDSIStructuredCDA = "urn:ihe:rad:CDA:ImagingReportStructuredHeadings:2013";

    /// <summary>
    /// Literal for code: RadiologyXDSIPDF
    /// </summary>
    public const string LiteralRadiologyXDSIPDF = "urn:ihe:rad:PDF";

    /// <summary>
    /// Literal for code: RadiologyXDSIText
    /// </summary>
    public const string LiteralRadiologyXDSIText = "urn:ihe:rad:TEXT";
  };
}
