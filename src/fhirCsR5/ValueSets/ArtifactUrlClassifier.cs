// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Code the reason for different URLs, eg abstract and full-text.
  /// </summary>
  public static class ArtifactUrlClassifierCodes
  {
    /// <summary>
    /// The URL will reach a brief summary for the article.
    /// </summary>
    public static readonly Coding Abstract = new Coding
    {
      Code = "abstract",
      Display = "Abstract",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// File archive and web hosting facility for source code of software, documentation, web pages, and other works.
    /// </summary>
    public static readonly Coding CodeRepository = new Coding
    {
      Code = "code-repository",
      Display = "Code repository",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// Compressed archive file (e.g. a zip file) that contains multiple files
    /// </summary>
    public static readonly Coding CompressedFile = new Coding
    {
      Code = "compressed-file",
      Display = "Compressed file",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// The URL will reach content that is machine-interpretable.
    /// </summary>
    public static readonly Coding ComputableResource = new Coding
    {
      Code = "computable-resource",
      Display = "Computable resource",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// The URL is derived from the Digital Object Identifier (DOI).
    /// </summary>
    public static readonly Coding DOIBased = new Coding
    {
      Code = "doi-based",
      Display = "DOI Based",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// The URL will reach a file directory.
    /// </summary>
    public static readonly Coding FileDirectory = new Coding
    {
      Code = "file-directory",
      Display = "File directory",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// The URL will reach the full-text of the article.
    /// </summary>
    public static readonly Coding FullText = new Coding
    {
      Code = "full-text",
      Display = "Full-Text",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// The URL will reach content in JSON format.
    /// </summary>
    public static readonly Coding JSON = new Coding
    {
      Code = "json",
      Display = "JSON",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// Used when URL classifier is not specified but expected in a system.
    /// </summary>
    public static readonly Coding NotSpecified = new Coding
    {
      Code = "not-specified",
      Display = "Not Specified",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// The URL will reach content in PDF form.
    /// </summary>
    public static readonly Coding PDF = new Coding
    {
      Code = "pdf",
      Display = "PDF",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// The URL content has restricted access (e.g. subcription required).
    /// </summary>
    public static readonly Coding Restricted = new Coding
    {
      Code = "restricted",
      Display = "Restricted",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// The URL will reach a supplement, appendix, or additional supporting information for the article.
    /// </summary>
    public static readonly Coding Supplement = new Coding
    {
      Code = "supplement",
      Display = "Supplement",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// The URL will reach content that is a specific version of the article.
    /// </summary>
    public static readonly Coding VersionSpecific = new Coding
    {
      Code = "version-specific",
      Display = "Version Specific",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// The URL will reach a webpage related to the article, where the content is not easily classified as abstract, full-text or supplement.
    /// </summary>
    public static readonly Coding Webpage = new Coding
    {
      Code = "webpage",
      Display = "Webpage",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };
    /// <summary>
    /// The URL will reach content in XML format.
    /// </summary>
    public static readonly Coding XML = new Coding
    {
      Code = "xml",
      Display = "XML",
      System = "http://terminology.hl7.org/CodeSystem/artifact-url-classifier"
    };

    /// <summary>
    /// Literal for code: Abstract
    /// </summary>
    public const string LiteralAbstract = "abstract";

    /// <summary>
    /// Literal for code: CodeRepository
    /// </summary>
    public const string LiteralCodeRepository = "code-repository";

    /// <summary>
    /// Literal for code: CompressedFile
    /// </summary>
    public const string LiteralCompressedFile = "compressed-file";

    /// <summary>
    /// Literal for code: ComputableResource
    /// </summary>
    public const string LiteralComputableResource = "computable-resource";

    /// <summary>
    /// Literal for code: DOIBased
    /// </summary>
    public const string LiteralDOIBased = "doi-based";

    /// <summary>
    /// Literal for code: FileDirectory
    /// </summary>
    public const string LiteralFileDirectory = "file-directory";

    /// <summary>
    /// Literal for code: FullText
    /// </summary>
    public const string LiteralFullText = "full-text";

    /// <summary>
    /// Literal for code: JSON
    /// </summary>
    public const string LiteralJSON = "json";

    /// <summary>
    /// Literal for code: NotSpecified
    /// </summary>
    public const string LiteralNotSpecified = "not-specified";

    /// <summary>
    /// Literal for code: PDF
    /// </summary>
    public const string LiteralPDF = "pdf";

    /// <summary>
    /// Literal for code: Restricted
    /// </summary>
    public const string LiteralRestricted = "restricted";

    /// <summary>
    /// Literal for code: Supplement
    /// </summary>
    public const string LiteralSupplement = "supplement";

    /// <summary>
    /// Literal for code: VersionSpecific
    /// </summary>
    public const string LiteralVersionSpecific = "version-specific";

    /// <summary>
    /// Literal for code: Webpage
    /// </summary>
    public const string LiteralWebpage = "webpage";

    /// <summary>
    /// Literal for code: XML
    /// </summary>
    public const string LiteralXML = "xml";
  };
}
