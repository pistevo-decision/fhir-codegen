// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// MedicationAdministration Category Codes
  /// </summary>
  public static class MedicationAdminCategoryCodes
  {
    /// <summary>
    /// Includes administrations by the patient in their home (this would include long term care or nursing homes, hospices, etc.)
    /// </summary>
    public static readonly Coding Community = new Coding
    {
      Code = "community",
      Display = "Community",
      System = "http://terminology.hl7.org/CodeSystem/medication-admin-category"
    };
    /// <summary>
    /// Includes administrations in an inpatient or acute care setting
    /// </summary>
    public static readonly Coding Inpatient = new Coding
    {
      Code = "inpatient",
      Display = "Inpatient",
      System = "http://terminology.hl7.org/CodeSystem/medication-admin-category"
    };
    /// <summary>
    /// Includes administrations in an outpatient setting (for example, Emergency Department, Outpatient Clinic, Outpatient Surgery, Doctor's office)
    /// </summary>
    public static readonly Coding Outpatient = new Coding
    {
      Code = "outpatient",
      Display = "Outpatient",
      System = "http://terminology.hl7.org/CodeSystem/medication-admin-category"
    };

    /// <summary>
    /// Literal for code: Community
    /// </summary>
    public const string LiteralCommunity = "community";

    /// <summary>
    /// Literal for code: MedicationAdminCategoryCommunity
    /// </summary>
    public const string LiteralMedicationAdminCategoryCommunity = "http://terminology.hl7.org/CodeSystem/medication-admin-category#community";

    /// <summary>
    /// Literal for code: Inpatient
    /// </summary>
    public const string LiteralInpatient = "inpatient";

    /// <summary>
    /// Literal for code: MedicationAdminCategoryInpatient
    /// </summary>
    public const string LiteralMedicationAdminCategoryInpatient = "http://terminology.hl7.org/CodeSystem/medication-admin-category#inpatient";

    /// <summary>
    /// Literal for code: Outpatient
    /// </summary>
    public const string LiteralOutpatient = "outpatient";

    /// <summary>
    /// Literal for code: MedicationAdminCategoryOutpatient
    /// </summary>
    public const string LiteralMedicationAdminCategoryOutpatient = "http://terminology.hl7.org/CodeSystem/medication-admin-category#outpatient";

    /// <summary>
    /// Dictionary for looking up MedicationAdminCategory Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "community", Community }, 
      { "http://terminology.hl7.org/CodeSystem/medication-admin-category#community", Community }, 
      { "inpatient", Inpatient }, 
      { "http://terminology.hl7.org/CodeSystem/medication-admin-category#inpatient", Inpatient }, 
      { "outpatient", Outpatient }, 
      { "http://terminology.hl7.org/CodeSystem/medication-admin-category#outpatient", Outpatient }, 
    };
  };
}