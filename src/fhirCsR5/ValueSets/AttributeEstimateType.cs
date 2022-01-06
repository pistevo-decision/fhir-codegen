// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// A statistic about a statistic, e.g.  Confidence interval or p-value
  /// </summary>
  public static class AttributeEstimateTypeCodes
  {
    /// <summary>
    /// The standard deviation of the sample-mean's estimate of a population mean. It is calculated by dividing the sample standard deviation (i.e., the sample-based estimate of the standard deviation of the population) by the square root of n , the size (number of observations) of the sample.
    /// </summary>
    public static readonly Coding StandardErrorOfTheMean = new Coding
    {
      Code = "0000037",
      Display = "Standard error of the mean",
      System = "http://terminology.hl7.org/CodeSystem/attribute-estimate-type"
    };
    /// <summary>
    /// A measure of heterogeneity across study computed by summing the squared deviations of each study's estimate from the overall meta-analytic estimate, weighting each study's contribution in the same manner as in the meta-analysis.
    /// </summary>
    public static readonly Coding CochranQuoteSQStatistic = new Coding
    {
      Code = "0000419",
      Display = "Cochran's Q statistic",
      System = "http://terminology.hl7.org/CodeSystem/attribute-estimate-type"
    };
    /// <summary>
    /// The percentage of total variation across studies that is due to heterogeneity rather than chance. I2 can be readily calculated from basic results obtained from a typical meta-analysis as i2 = 100%×(q - df)/q, where q is cochran's heterogeneity statistic and df the degrees of freedom. Negative values of i2 are put equal to zero so that i2 lies between 0% and 100%. A value of 0% indicates no observed heterogeneity, and larger values show increasing heterogeneity. Unlike cochran's q, it does not inherently depend upon the number of studies considered. A confidence interval for i² is constructed using either i) the iterative non-central chi-squared distribution method of hedges and piggott (2001); or ii) the test-based method of higgins and thompson (2002). The non-central chi-square method is currently the method of choice (higgins, personal communication, 2006) – it is computed if the 'exact' option is selected.
    /// </summary>
    public static readonly Coding ISquared = new Coding
    {
      Code = "0000420",
      Display = "I-squared",
      System = "http://terminology.hl7.org/CodeSystem/attribute-estimate-type"
    };
    /// <summary>
    /// An estimate of the between-study variance in a random-effects meta-analysis. The square root of this number (i.e. Tau) is the estimated standard deviation of underlying effects across studies.
    /// </summary>
    public static readonly Coding TauSquared = new Coding
    {
      Code = "0000421",
      Display = "Tau squared",
      System = "http://terminology.hl7.org/CodeSystem/attribute-estimate-type"
    };
    /// <summary>
    /// An interval of a posterior distribution which is such that the density at any point inside the interval is greater than the density at any point outside and that the area under the curve for that interval is equal to a prespecified probability level. For any probability level there is generally only one such interval, which is also often known as the highest posterior density region. Unlike the usual confidence interval associated with frequentist inference, here the intervals specify the range within which parameters lie with a certain probability. The bayesian counterparts of the confidence interval used in frequentists statistics.
    /// </summary>
    public static readonly Coding CredibleInterval = new Coding
    {
      Code = "0000455",
      Display = "Credible interval",
      System = "http://terminology.hl7.org/CodeSystem/attribute-estimate-type"
    };
    /// <summary>
    /// The difference between the lowest and highest numerical values; the limits or scale of variation.
    /// </summary>
    public static readonly Coding Range = new Coding
    {
      Code = "C38013",
      Display = "Range",
      System = "http://terminology.hl7.org/CodeSystem/attribute-estimate-type"
    };
    /// <summary>
    /// The probability of obtaining the results obtained, or more extreme results, if the hypothesis being tested and all other model assumptions are true
    /// </summary>
    public static readonly Coding PValue = new Coding
    {
      Code = "C44185",
      Display = "P-value",
      System = "http://terminology.hl7.org/CodeSystem/attribute-estimate-type"
    };
    /// <summary>
    /// A measure of the variability in a sample or population. It is calculated as the mean squared deviation (MSD) of the individual values from their common mean. In calculating the MSD, the divisor n is commonly used for a population variance and the divisor n-1 for a sample variance.
    /// </summary>
    public static readonly Coding Variance = new Coding
    {
      Code = "C48918",
      Display = "Variance",
      System = "http://terminology.hl7.org/CodeSystem/attribute-estimate-type"
    };
    /// <summary>
    /// The difference between the 3d and 1st quartiles is called the interquartile range and it is used as a measure of variability (dispersion).
    /// </summary>
    public static readonly Coding InterquartileRange = new Coding
    {
      Code = "C53245",
      Display = "Interquartile range",
      System = "http://terminology.hl7.org/CodeSystem/attribute-estimate-type"
    };
    /// <summary>
    /// A measure of the range of values in a set of numbers. Standard deviation is a statistic used as a measure of the dispersion or variation in a distribution, equal to the square root of the arithmetic mean of the squares of the deviations from the arithmetic mean.
    /// </summary>
    public static readonly Coding StandardDeviation = new Coding
    {
      Code = "C53322",
      Display = "Standard deviation",
      System = "http://terminology.hl7.org/CodeSystem/attribute-estimate-type"
    };
    /// <summary>
    /// A range of values considered compatible with the observed data at the specified confidence level
    /// </summary>
    public static readonly Coding ConfidenceInterval = new Coding
    {
      Code = "C53324",
      Display = "Confidence interval",
      System = "http://terminology.hl7.org/CodeSystem/attribute-estimate-type"
    };

    /// <summary>
    /// Literal for code: StandardErrorOfTheMean
    /// </summary>
    public const string LiteralStandardErrorOfTheMean = "0000037";

    /// <summary>
    /// Literal for code: CochranQuoteSQStatistic
    /// </summary>
    public const string LiteralCochranQuoteSQStatistic = "0000419";

    /// <summary>
    /// Literal for code: ISquared
    /// </summary>
    public const string LiteralISquared = "0000420";

    /// <summary>
    /// Literal for code: TauSquared
    /// </summary>
    public const string LiteralTauSquared = "0000421";

    /// <summary>
    /// Literal for code: CredibleInterval
    /// </summary>
    public const string LiteralCredibleInterval = "0000455";

    /// <summary>
    /// Literal for code: Range
    /// </summary>
    public const string LiteralRange = "C38013";

    /// <summary>
    /// Literal for code: PValue
    /// </summary>
    public const string LiteralPValue = "C44185";

    /// <summary>
    /// Literal for code: Variance
    /// </summary>
    public const string LiteralVariance = "C48918";

    /// <summary>
    /// Literal for code: InterquartileRange
    /// </summary>
    public const string LiteralInterquartileRange = "C53245";

    /// <summary>
    /// Literal for code: StandardDeviation
    /// </summary>
    public const string LiteralStandardDeviation = "C53322";

    /// <summary>
    /// Literal for code: ConfidenceInterval
    /// </summary>
    public const string LiteralConfidenceInterval = "C53324";
  };
}
