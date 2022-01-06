// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Example value set for investigation type
  /// </summary>
  public static class InvestigationSetsCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding HistorySymptoms = new Coding
    {
      Code = "160237006",
      Display = "History/symptoms",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding ExaminationSigns = new Coding
    {
      Code = "271336007",
      Display = "Examination / signs",
      System = "http://snomed.info/sct"
    };

    /// <summary>
    /// Literal for code: HistorySymptoms
    /// </summary>
    public const string LiteralHistorySymptoms = "160237006";

    /// <summary>
    /// Literal for code: ExaminationSigns
    /// </summary>
    public const string LiteralExaminationSigns = "271336007";
  };
}
