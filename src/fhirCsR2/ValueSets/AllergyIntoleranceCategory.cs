// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// Category of an identified Substance.
  /// </summary>
  public static class AllergyIntoleranceCategoryCodes
  {
    /// <summary>
    /// Substances that are encountered in the environment.
    /// </summary>
    public static readonly Coding Environment = new Coding
    {
      Code = "environment",
      Display = "Environment",
      System = "http://hl7.org/fhir/allergy-intolerance-category"
    };
    /// <summary>
    /// Any substance consumed to provide nutritional support for the body.
    /// </summary>
    public static readonly Coding Food = new Coding
    {
      Code = "food",
      Display = "Food",
      System = "http://hl7.org/fhir/allergy-intolerance-category"
    };
    /// <summary>
    /// Substances administered to achieve a physiological effect.
    /// </summary>
    public static readonly Coding Medication = new Coding
    {
      Code = "medication",
      Display = "Medication",
      System = "http://hl7.org/fhir/allergy-intolerance-category"
    };
    /// <summary>
    /// Other substances that are not covered by any other category.
    /// </summary>
    public static readonly Coding Other = new Coding
    {
      Code = "other",
      Display = "Other",
      System = "http://hl7.org/fhir/allergy-intolerance-category"
    };

    /// <summary>
    /// Literal for code: Environment
    /// </summary>
    public const string LiteralEnvironment = "environment";

    /// <summary>
    /// Literal for code: Food
    /// </summary>
    public const string LiteralFood = "food";

    /// <summary>
    /// Literal for code: Medication
    /// </summary>
    public const string LiteralMedication = "medication";

    /// <summary>
    /// Literal for code: Other
    /// </summary>
    public const string LiteralOther = "other";
  };
}
