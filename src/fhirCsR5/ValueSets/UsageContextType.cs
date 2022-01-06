// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// A code that specifies a type of context being specified by a usage context.
  /// </summary>
  public static class UsageContextTypeCodes
  {
    /// <summary>
    /// The age of the patient. For this context type, the value could be a range that specifies the applicable ages or a code from an appropriate value set such as the MeSH value set http://terminology.hl7.org/ValueSet/v3-AgeGroupObservationValue.
    /// </summary>
    public static readonly Coding AgeRange = new Coding
    {
      Code = "age",
      Display = "Age Range",
      System = "http://terminology.hl7.org/CodeSystem/usage-context-type"
    };
    /// <summary>
    /// The clinical concept(s) addressed by the artifact. For example, disease, diagnostic test interpretation, medication ordering as in http://hl7.org/fhir/ValueSet/condition-code.
    /// </summary>
    public static readonly Coding ClinicalFocus = new Coding
    {
      Code = "focus",
      Display = "Clinical Focus",
      System = "http://terminology.hl7.org/CodeSystem/usage-context-type"
    };
    /// <summary>
    /// The gender of the patient. For this context type, appropriate values can be found in the http://hl7.org/fhir/ValueSet/administrative-gender value set.
    /// </summary>
    public static readonly Coding Gender = new Coding
    {
      Code = "gender",
      Display = "Gender",
      System = "http://terminology.hl7.org/CodeSystem/usage-context-type"
    };
    /// <summary>
    /// A program/project of work for which this artifact is applicable.
    /// </summary>
    public static readonly Coding Program = new Coding
    {
      Code = "program",
      Display = "Program",
      System = "http://terminology.hl7.org/CodeSystem/usage-context-type"
    };
    /// <summary>
    /// The species to which an artifact applies. For example, SNOMED - 387961004 | Kingdom Animalia (organism).
    /// </summary>
    public static readonly Coding Species = new Coding
    {
      Code = "species",
      Display = "Species",
      System = "http://terminology.hl7.org/CodeSystem/usage-context-type"
    };
    /// <summary>
    /// The context for the clinical task(s) represented by this artifact. For example, this could be any task context represented by the HL7 ActTaskCode value set http://terminology.hl7.org/ValueSet/v3-ActTaskCode. General categories include: order entry, patient documentation and patient information review.
    /// </summary>
    public static readonly Coding WorkflowTask = new Coding
    {
      Code = "task",
      Display = "Workflow Task",
      System = "http://terminology.hl7.org/CodeSystem/usage-context-type"
    };
    /// <summary>
    /// The clinical specialty of the context in which the patient is being treated - For example, PCP, Patient, Cardiologist, Behavioral Professional, Oral Health Professional, Prescriber, etc... taken from a specialty value set such as the NUCC Health Care provider taxonomy value set http://hl7.org/fhir/ValueSet/provider-taxonomy.
    /// </summary>
    public static readonly Coding UserType = new Coding
    {
      Code = "user",
      Display = "User Type",
      System = "http://terminology.hl7.org/CodeSystem/usage-context-type"
    };
    /// <summary>
    /// The venue in which an artifact could be used. For example, Outpatient, Inpatient, Home, Nursing home. The code value may originate from the HL7 ServiceDeliveryLocationRoleType value set (http://terminology.hl7.org/ValueSet/v3-ServiceDeliveryLocationRoleType).
    /// </summary>
    public static readonly Coding ClinicalVenue = new Coding
    {
      Code = "venue",
      Display = "Clinical Venue",
      System = "http://terminology.hl7.org/CodeSystem/usage-context-type"
    };
    /// <summary>
    /// The settings in which the artifact is intended for use. For example, admission, pre-op, etc. For example, the ActEncounterCode value set http://terminology.hl7.org/ValueSet/v3-ActEncounterCode.
    /// </summary>
    public static readonly Coding WorkflowSetting = new Coding
    {
      Code = "workflow",
      Display = "Workflow Setting",
      System = "http://terminology.hl7.org/CodeSystem/usage-context-type"
    };

    /// <summary>
    /// Literal for code: AgeRange
    /// </summary>
    public const string LiteralAgeRange = "age";

    /// <summary>
    /// Literal for code: ClinicalFocus
    /// </summary>
    public const string LiteralClinicalFocus = "focus";

    /// <summary>
    /// Literal for code: Gender
    /// </summary>
    public const string LiteralGender = "gender";

    /// <summary>
    /// Literal for code: Program
    /// </summary>
    public const string LiteralProgram = "program";

    /// <summary>
    /// Literal for code: Species
    /// </summary>
    public const string LiteralSpecies = "species";

    /// <summary>
    /// Literal for code: WorkflowTask
    /// </summary>
    public const string LiteralWorkflowTask = "task";

    /// <summary>
    /// Literal for code: UserType
    /// </summary>
    public const string LiteralUserType = "user";

    /// <summary>
    /// Literal for code: ClinicalVenue
    /// </summary>
    public const string LiteralClinicalVenue = "venue";

    /// <summary>
    /// Literal for code: WorkflowSetting
    /// </summary>
    public const string LiteralWorkflowSetting = "workflow";
  };
}
