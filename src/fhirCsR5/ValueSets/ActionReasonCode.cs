// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Provides examples of reasons for actions to be performed.
  /// </summary>
  public static class ActionReasonCodeCodes
  {
    /// <summary>
    /// The action should be performed to address a detected care gap.
    /// </summary>
    public static readonly Coding CareGapDetected = new Coding
    {
      Code = "care-gap",
      Display = "Care gap detected",
      System = "http://terminology.hl7.org/CodeSystem/action-reason-code"
    };
    /// <summary>
    /// The action should be performed to address a detected potential drug-drug interaction.
    /// </summary>
    public static readonly Coding DrugDrugInteraction = new Coding
    {
      Code = "drug-drug-interaction",
      Display = "Drug-drug interaction",
      System = "http://terminology.hl7.org/CodeSystem/action-reason-code"
    };
    /// <summary>
    /// The action should be performed because the patient was determined to be off pathway.
    /// </summary>
    public static readonly Coding OffPathway = new Coding
    {
      Code = "off-pathway",
      Display = "Off pathway",
      System = "http://terminology.hl7.org/CodeSystem/action-reason-code"
    };
    /// <summary>
    /// The action should be performed to bring the patient's care in line with a quality measure.
    /// </summary>
    public static readonly Coding QualityMeasure = new Coding
    {
      Code = "quality-measure",
      Display = "Quality measure",
      System = "http://terminology.hl7.org/CodeSystem/action-reason-code"
    };
    /// <summary>
    /// The action should be performed based on a particular risk assessment.
    /// </summary>
    public static readonly Coding RiskAssessment = new Coding
    {
      Code = "risk-assessment",
      Display = "Risk assessment",
      System = "http://terminology.hl7.org/CodeSystem/action-reason-code"
    };

    /// <summary>
    /// Literal for code: CareGapDetected
    /// </summary>
    public const string LiteralCareGapDetected = "care-gap";

    /// <summary>
    /// Literal for code: DrugDrugInteraction
    /// </summary>
    public const string LiteralDrugDrugInteraction = "drug-drug-interaction";

    /// <summary>
    /// Literal for code: OffPathway
    /// </summary>
    public const string LiteralOffPathway = "off-pathway";

    /// <summary>
    /// Literal for code: QualityMeasure
    /// </summary>
    public const string LiteralQualityMeasure = "quality-measure";

    /// <summary>
    /// Literal for code: RiskAssessment
    /// </summary>
    public const string LiteralRiskAssessment = "risk-assessment";
  };
}
