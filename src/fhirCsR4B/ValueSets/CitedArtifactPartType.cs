// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// To describe the reason for the variant citation, such as version number or subpart specification.
  /// </summary>
  public static class CitedArtifactPartTypeCodes
  {
    /// <summary>
    /// Used to distinguish an individual article within an article set where the article set is a base citation.
    /// </summary>
    public static readonly Coding PartOfAnArticleSet = new Coding
    {
      Code = "article-set",
      Display = "Part of an article set",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-part-type"
    };
    /// <summary>
    /// Denotes specific figure or figures of an article or artifact.
    /// </summary>
    public static readonly Coding Figures = new Coding
    {
      Code = "figures",
      Display = "figures",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-part-type"
    };
    /// <summary>
    /// Denotes specific line or lines of an article or artifact.
    /// </summary>
    public static readonly Coding Lines = new Coding
    {
      Code = "lines",
      Display = "lines",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-part-type"
    };
    /// <summary>
    /// Denotes specific page or pages of an article or artifact.
    /// </summary>
    public static readonly Coding Pages = new Coding
    {
      Code = "pages",
      Display = "pages",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-part-type"
    };
    /// <summary>
    /// Denotes specific paragraph or paragraphs of an article or artifact.
    /// </summary>
    public static readonly Coding Paragraphs = new Coding
    {
      Code = "paragraphs",
      Display = "paragraphs",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-part-type"
    };
    /// <summary>
    /// Denotes specific section or sections of an article or artifact.
    /// </summary>
    public static readonly Coding Sections = new Coding
    {
      Code = "sections",
      Display = "sections",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-part-type"
    };
    /// <summary>
    /// Used to denote a supplementary file, appendix, or additional part that is not a subpart of the primary article.
    /// </summary>
    public static readonly Coding SupplementOrAppendix = new Coding
    {
      Code = "supplement",
      Display = "Supplement or Appendix",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-part-type"
    };
    /// <summary>
    /// Used to denote a subpart within a supplementary file or appendix.
    /// </summary>
    public static readonly Coding SupplementOrAppendixSubpart = new Coding
    {
      Code = "supplement-subpart",
      Display = "Supplement or Appendix Subpart",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-part-type"
    };
    /// <summary>
    /// Denotes specific table or tables of an article or artifact.
    /// </summary>
    public static readonly Coding Tables = new Coding
    {
      Code = "tables",
      Display = "tables",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-part-type"
    };

    /// <summary>
    /// Literal for code: PartOfAnArticleSet
    /// </summary>
    public const string LiteralPartOfAnArticleSet = "article-set";

    /// <summary>
    /// Literal for code: Figures
    /// </summary>
    public const string LiteralFigures = "figures";

    /// <summary>
    /// Literal for code: Lines
    /// </summary>
    public const string LiteralLines = "lines";

    /// <summary>
    /// Literal for code: Pages
    /// </summary>
    public const string LiteralPages = "pages";

    /// <summary>
    /// Literal for code: Paragraphs
    /// </summary>
    public const string LiteralParagraphs = "paragraphs";

    /// <summary>
    /// Literal for code: Sections
    /// </summary>
    public const string LiteralSections = "sections";

    /// <summary>
    /// Literal for code: SupplementOrAppendix
    /// </summary>
    public const string LiteralSupplementOrAppendix = "supplement";

    /// <summary>
    /// Literal for code: SupplementOrAppendixSubpart
    /// </summary>
    public const string LiteralSupplementOrAppendixSubpart = "supplement-subpart";

    /// <summary>
    /// Literal for code: Tables
    /// </summary>
    public const string LiteralTables = "tables";
  };
}
