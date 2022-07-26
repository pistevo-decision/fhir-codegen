// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// This value set includes sample Provider Qualification codes.
  /// </summary>
  public static class ProviderQualificationCodes
  {
    /// <summary>
    /// Dentist
    /// </summary>
    public static readonly Coding Dentist = new Coding
    {
      Code = "311405",
      Display = "Dentist",
      System = "http://terminology.hl7.org/CodeSystem/ex-providerqualification"
    };
    /// <summary>
    /// Optometrist
    /// </summary>
    public static readonly Coding Optometrist = new Coding
    {
      Code = "604210",
      Display = "Optometrist",
      System = "http://terminology.hl7.org/CodeSystem/ex-providerqualification"
    };
    /// <summary>
    /// Ophthalmologist
    /// </summary>
    public static readonly Coding Ophthalmologist = new Coding
    {
      Code = "604215",
      Display = "Ophthalmologist",
      System = "http://terminology.hl7.org/CodeSystem/ex-providerqualification"
    };

    /// <summary>
    /// Literal for code: Dentist
    /// </summary>
    public const string LiteralDentist = "311405";

    /// <summary>
    /// Literal for code: ExProviderqualificationDentist
    /// </summary>
    public const string LiteralExProviderqualificationDentist = "http://terminology.hl7.org/CodeSystem/ex-providerqualification#311405";

    /// <summary>
    /// Literal for code: Optometrist
    /// </summary>
    public const string LiteralOptometrist = "604210";

    /// <summary>
    /// Literal for code: ExProviderqualificationOptometrist
    /// </summary>
    public const string LiteralExProviderqualificationOptometrist = "http://terminology.hl7.org/CodeSystem/ex-providerqualification#604210";

    /// <summary>
    /// Literal for code: Ophthalmologist
    /// </summary>
    public const string LiteralOphthalmologist = "604215";

    /// <summary>
    /// Literal for code: ExProviderqualificationOphthalmologist
    /// </summary>
    public const string LiteralExProviderqualificationOphthalmologist = "http://terminology.hl7.org/CodeSystem/ex-providerqualification#604215";

    /// <summary>
    /// Dictionary for looking up ProviderQualification Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "311405", Dentist }, 
      { "http://terminology.hl7.org/CodeSystem/ex-providerqualification#311405", Dentist }, 
      { "604210", Optometrist }, 
      { "http://terminology.hl7.org/CodeSystem/ex-providerqualification#604210", Optometrist }, 
      { "604215", Ophthalmologist }, 
      { "http://terminology.hl7.org/CodeSystem/ex-providerqualification#604215", Ophthalmologist }, 
    };
  };
}