// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// This value set includes Claim Type codes.
  /// </summary>
  public static class ClaimTypeCodes
  {
    /// <summary>
    /// Hospital, clinic and typically inpatient claims.
    /// </summary>
    public static readonly Coding Institutional = new Coding
    {
      Code = "institutional",
      Display = "Institutional",
      System = "http://terminology.hl7.org/CodeSystem/claim-type"
    };
    /// <summary>
    /// Dental, Denture and Hygiene claims.
    /// </summary>
    public static readonly Coding Oral = new Coding
    {
      Code = "oral",
      Display = "Oral",
      System = "http://terminology.hl7.org/CodeSystem/claim-type"
    };
    /// <summary>
    /// Pharmacy claims for goods and services.
    /// </summary>
    public static readonly Coding Pharmacy = new Coding
    {
      Code = "pharmacy",
      Display = "Pharmacy",
      System = "http://terminology.hl7.org/CodeSystem/claim-type"
    };
    /// <summary>
    /// Typically, outpatient claims from Physician, Psychological, Chiropractor, Physiotherapy, Speech Pathology, rehabilitative, consulting.
    /// </summary>
    public static readonly Coding Professional = new Coding
    {
      Code = "professional",
      Display = "Professional",
      System = "http://terminology.hl7.org/CodeSystem/claim-type"
    };
    /// <summary>
    /// Vision claims for professional services and products such as glasses and contact lenses.
    /// </summary>
    public static readonly Coding Vision = new Coding
    {
      Code = "vision",
      Display = "Vision",
      System = "http://terminology.hl7.org/CodeSystem/claim-type"
    };

    /// <summary>
    /// Literal for code: Institutional
    /// </summary>
    public const string LiteralInstitutional = "institutional";

    /// <summary>
    /// Literal for code: Oral
    /// </summary>
    public const string LiteralOral = "oral";

    /// <summary>
    /// Literal for code: Pharmacy
    /// </summary>
    public const string LiteralPharmacy = "pharmacy";

    /// <summary>
    /// Literal for code: Professional
    /// </summary>
    public const string LiteralProfessional = "professional";

    /// <summary>
    /// Literal for code: Vision
    /// </summary>
    public const string LiteralVision = "vision";
  };
}
