// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Codes representing the likelihood of a particular outcome in a risk assessment.
  /// </summary>
  public static class RiskProbabilityCodes
  {
    /// <summary>
    /// The specified outcome is effectively guaranteed.
    /// </summary>
    public static readonly Coding Certain = new Coding
    {
      Code = "certain",
      Display = "Certain",
      System = "http://terminology.hl7.org/CodeSystem/risk-probability"
    };
    /// <summary>
    /// The specified outcome is more likely to occur than not.
    /// </summary>
    public static readonly Coding HighLikelihood = new Coding
    {
      Code = "high",
      Display = "High likelihood",
      System = "http://terminology.hl7.org/CodeSystem/risk-probability"
    };
    /// <summary>
    /// The specified outcome is possible but unlikely.
    /// </summary>
    public static readonly Coding LowLikelihood = new Coding
    {
      Code = "low",
      Display = "Low likelihood",
      System = "http://terminology.hl7.org/CodeSystem/risk-probability"
    };
    /// <summary>
    /// The specified outcome has a reasonable likelihood of occurrence.
    /// </summary>
    public static readonly Coding ModerateLikelihood = new Coding
    {
      Code = "moderate",
      Display = "Moderate likelihood",
      System = "http://terminology.hl7.org/CodeSystem/risk-probability"
    };
    /// <summary>
    /// The specified outcome is exceptionally unlikely.
    /// </summary>
    public static readonly Coding NegligibleLikelihood = new Coding
    {
      Code = "negligible",
      Display = "Negligible likelihood",
      System = "http://terminology.hl7.org/CodeSystem/risk-probability"
    };

    /// <summary>
    /// Literal for code: Certain
    /// </summary>
    public const string LiteralCertain = "certain";

    /// <summary>
    /// Literal for code: HighLikelihood
    /// </summary>
    public const string LiteralHighLikelihood = "high";

    /// <summary>
    /// Literal for code: LowLikelihood
    /// </summary>
    public const string LiteralLowLikelihood = "low";

    /// <summary>
    /// Literal for code: ModerateLikelihood
    /// </summary>
    public const string LiteralModerateLikelihood = "moderate";

    /// <summary>
    /// Literal for code: NegligibleLikelihood
    /// </summary>
    public const string LiteralNegligibleLikelihood = "negligible";
  };
}
