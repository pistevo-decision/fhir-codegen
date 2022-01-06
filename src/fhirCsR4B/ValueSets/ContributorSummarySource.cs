// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// Used to code the producer or rule for creating the display string.
  /// </summary>
  public static class ContributorSummarySourceCodes
  {
    /// <summary>
    /// Data copied by human from article text.
    /// </summary>
    public static readonly Coding CopiedFromArticle = new Coding
    {
      Code = "article-copy",
      Display = "Copied from article",
      System = "http://terminology.hl7.org/CodeSystem/contributor-summary-source"
    };
    /// <summary>
    /// Data copied by machine from citation manager data.
    /// </summary>
    public static readonly Coding ReportedByCitationManager = new Coding
    {
      Code = "citation-manager",
      Display = "Reported by citation manager",
      System = "http://terminology.hl7.org/CodeSystem/contributor-summary-source"
    };
    /// <summary>
    /// Custom format (may be described in text note).
    /// </summary>
    public static readonly Coding CustomFormat = new Coding
    {
      Code = "custom",
      Display = "custom format",
      System = "http://terminology.hl7.org/CodeSystem/contributor-summary-source"
    };
    /// <summary>
    /// Data copied by machine from publisher data.
    /// </summary>
    public static readonly Coding PublisherProvided = new Coding
    {
      Code = "publisher-data",
      Display = "Publisher provided",
      System = "http://terminology.hl7.org/CodeSystem/contributor-summary-source"
    };

    /// <summary>
    /// Literal for code: CopiedFromArticle
    /// </summary>
    public const string LiteralCopiedFromArticle = "article-copy";

    /// <summary>
    /// Literal for code: ReportedByCitationManager
    /// </summary>
    public const string LiteralReportedByCitationManager = "citation-manager";

    /// <summary>
    /// Literal for code: CustomFormat
    /// </summary>
    public const string LiteralCustomFormat = "custom";

    /// <summary>
    /// Literal for code: PublisherProvided
    /// </summary>
    public const string LiteralPublisherProvided = "publisher-data";
  };
}
