// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The method used to determine the characteristic(s) of the variable.
  /// </summary>
  public static class CharacteristicMethodCodes
  {
    /// <summary>
    /// Any non-systematic method for determining whether or not adverse events have occurred, such as self-reporting by participants or occasional assessment/testing
    /// </summary>
    public static readonly Coding NonSystematicAssessment = new Coding
    {
      Code = "non-systematic-assessment",
      Display = "Non-Systematic Assessment",
      System = "http://terminology.hl7.org/CodeSystem/characteristic-method"
    };
    /// <summary>
    /// Any method of routinely determining whether or not certain adverse events have occurred, for example through a standard questionnaire, regular investigator assessment, regular laboratory testing, or other method
    /// </summary>
    public static readonly Coding SystematicAssessment = new Coding
    {
      Code = "systematic-assessment",
      Display = "Systematic Assessment",
      System = "http://terminology.hl7.org/CodeSystem/characteristic-method"
    };

    /// <summary>
    /// Literal for code: NonSystematicAssessment
    /// </summary>
    public const string LiteralNonSystematicAssessment = "non-systematic-assessment";

    /// <summary>
    /// Literal for code: SystematicAssessment
    /// </summary>
    public const string LiteralSystematicAssessment = "systematic-assessment";
  };
}
