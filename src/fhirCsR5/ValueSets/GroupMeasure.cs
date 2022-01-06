// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Possible group measure aggregates (E.g. Mean, Median).
  /// </summary>
  public static class GroupMeasureCodes
  {
    /// <summary>
    /// Aggregated using Mean of participant values.
    /// </summary>
    public static readonly Coding Mean = new Coding
    {
      Code = "mean",
      Display = "Mean",
      System = "http://hl7.org/fhir/group-measure"
    };
    /// <summary>
    /// Aggregated using Mean of study mean values.
    /// </summary>
    public static readonly Coding MeanOfStudyMeans = new Coding
    {
      Code = "mean-of-mean",
      Display = "Mean of Study Means",
      System = "http://hl7.org/fhir/group-measure"
    };
    /// <summary>
    /// Aggregated using Mean of study median values.
    /// </summary>
    public static readonly Coding MeanOfStudyMedins = new Coding
    {
      Code = "mean-of-median",
      Display = "Mean of Study Medins",
      System = "http://hl7.org/fhir/group-measure"
    };
    /// <summary>
    /// Aggregated using Median of participant values.
    /// </summary>
    public static readonly Coding Median = new Coding
    {
      Code = "median",
      Display = "Median",
      System = "http://hl7.org/fhir/group-measure"
    };
    /// <summary>
    /// Aggregated using Median of study mean values.
    /// </summary>
    public static readonly Coding MedianOfStudyMeans = new Coding
    {
      Code = "median-of-mean",
      Display = "Median of Study Means",
      System = "http://hl7.org/fhir/group-measure"
    };
    /// <summary>
    /// Aggregated using Median of study median values.
    /// </summary>
    public static readonly Coding MedianOfStudyMedians = new Coding
    {
      Code = "median-of-median",
      Display = "Median of Study Medians",
      System = "http://hl7.org/fhir/group-measure"
    };

    /// <summary>
    /// Literal for code: Mean
    /// </summary>
    public const string LiteralMean = "mean";

    /// <summary>
    /// Literal for code: MeanOfStudyMeans
    /// </summary>
    public const string LiteralMeanOfStudyMeans = "mean-of-mean";

    /// <summary>
    /// Literal for code: MeanOfStudyMedins
    /// </summary>
    public const string LiteralMeanOfStudyMedins = "mean-of-median";

    /// <summary>
    /// Literal for code: Median
    /// </summary>
    public const string LiteralMedian = "median";

    /// <summary>
    /// Literal for code: MedianOfStudyMeans
    /// </summary>
    public const string LiteralMedianOfStudyMeans = "median-of-mean";

    /// <summary>
    /// Literal for code: MedianOfStudyMedians
    /// </summary>
    public const string LiteralMedianOfStudyMedians = "median-of-median";
  };
}
