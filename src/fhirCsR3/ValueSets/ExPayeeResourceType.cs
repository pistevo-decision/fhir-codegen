// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// The type of Claim payee Resource
  /// </summary>
  public static class ExPayeeResourceTypeCodes
  {
    /// <summary>
    /// Organization resource
    /// </summary>
    public static readonly Coding Organization = new Coding
    {
      Code = "organization",
      Display = "Organization",
      System = "http://hl7.org/fhir/ex-payee-resource-type"
    };
    /// <summary>
    /// Patient resource
    /// </summary>
    public static readonly Coding Patient = new Coding
    {
      Code = "patient",
      Display = "Patient",
      System = "http://hl7.org/fhir/ex-payee-resource-type"
    };
    /// <summary>
    /// Practitioner resource
    /// </summary>
    public static readonly Coding Practitioner = new Coding
    {
      Code = "practitioner",
      Display = "Practitioner",
      System = "http://hl7.org/fhir/ex-payee-resource-type"
    };
    /// <summary>
    /// RelatedPerson resource
    /// </summary>
    public static readonly Coding RelatedPerson = new Coding
    {
      Code = "relatedperson",
      Display = "RelatedPerson",
      System = "http://hl7.org/fhir/ex-payee-resource-type"
    };

    /// <summary>
    /// Literal for code: Organization
    /// </summary>
    public const string LiteralOrganization = "organization";

    /// <summary>
    /// Literal for code: ExPayeeResourceTypeOrganization
    /// </summary>
    public const string LiteralExPayeeResourceTypeOrganization = "http://hl7.org/fhir/ex-payee-resource-type#organization";

    /// <summary>
    /// Literal for code: Patient
    /// </summary>
    public const string LiteralPatient = "patient";

    /// <summary>
    /// Literal for code: ExPayeeResourceTypePatient
    /// </summary>
    public const string LiteralExPayeeResourceTypePatient = "http://hl7.org/fhir/ex-payee-resource-type#patient";

    /// <summary>
    /// Literal for code: Practitioner
    /// </summary>
    public const string LiteralPractitioner = "practitioner";

    /// <summary>
    /// Literal for code: ExPayeeResourceTypePractitioner
    /// </summary>
    public const string LiteralExPayeeResourceTypePractitioner = "http://hl7.org/fhir/ex-payee-resource-type#practitioner";

    /// <summary>
    /// Literal for code: RelatedPerson
    /// </summary>
    public const string LiteralRelatedPerson = "relatedperson";

    /// <summary>
    /// Literal for code: ExPayeeResourceTypeRelatedPerson
    /// </summary>
    public const string LiteralExPayeeResourceTypeRelatedPerson = "http://hl7.org/fhir/ex-payee-resource-type#relatedperson";

    /// <summary>
    /// Dictionary for looking up ExPayeeResourceType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "organization", Organization }, 
      { "http://hl7.org/fhir/ex-payee-resource-type#organization", Organization }, 
      { "patient", Patient }, 
      { "http://hl7.org/fhir/ex-payee-resource-type#patient", Patient }, 
      { "practitioner", Practitioner }, 
      { "http://hl7.org/fhir/ex-payee-resource-type#practitioner", Practitioner }, 
      { "relatedperson", RelatedPerson }, 
      { "http://hl7.org/fhir/ex-payee-resource-type#relatedperson", RelatedPerson }, 
    };
  };
}