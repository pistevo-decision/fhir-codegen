// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The type of relationship between reports.
  /// </summary>
  public static class ReportRelationTypeCodes
  {
    /// <summary>
    /// This document was.
    /// </summary>
    public static readonly Coding AmendedWith = new Coding
    {
      Code = "amendedWith",
      Display = "Amended With",
      System = "http://hl7.org/fhir/report-relation-type"
    };
    /// <summary>
    /// This document notes corrections or changes to replace or supersede parts of the target document.
    /// </summary>
    public static readonly Coding Amends = new Coding
    {
      Code = "amends",
      Display = "Amends",
      System = "http://hl7.org/fhir/report-relation-type"
    };
    /// <summary>
    /// This document was.
    /// </summary>
    public static readonly Coding AppendedWith = new Coding
    {
      Code = "appendedWith",
      Display = "Appended With",
      System = "http://hl7.org/fhir/report-relation-type"
    };
    /// <summary>
    /// This document adds additional information to the target document.
    /// </summary>
    public static readonly Coding Appends = new Coding
    {
      Code = "appends",
      Display = "Appends",
      System = "http://hl7.org/fhir/report-relation-type"
    };
    /// <summary>
    /// This document was.
    /// </summary>
    public static readonly Coding ReplacedWith = new Coding
    {
      Code = "replacedWith",
      Display = "Replaced With",
      System = "http://hl7.org/fhir/report-relation-type"
    };
    /// <summary>
    /// This document replaces or supersedes the target document.
    /// </summary>
    public static readonly Coding Replaces = new Coding
    {
      Code = "replaces",
      Display = "Replaces",
      System = "http://hl7.org/fhir/report-relation-type"
    };
    /// <summary>
    /// This document was.
    /// </summary>
    public static readonly Coding TransformedWith = new Coding
    {
      Code = "transformedWith",
      Display = "Transformed With",
      System = "http://hl7.org/fhir/report-relation-type"
    };
    /// <summary>
    /// This document was generated by transforming the target document (eg format or language conversion).
    /// </summary>
    public static readonly Coding Transforms = new Coding
    {
      Code = "transforms",
      Display = "Transforms",
      System = "http://hl7.org/fhir/report-relation-type"
    };

    /// <summary>
    /// Literal for code: AmendedWith
    /// </summary>
    public const string LiteralAmendedWith = "amendedWith";

    /// <summary>
    /// Literal for code: Amends
    /// </summary>
    public const string LiteralAmends = "amends";

    /// <summary>
    /// Literal for code: AppendedWith
    /// </summary>
    public const string LiteralAppendedWith = "appendedWith";

    /// <summary>
    /// Literal for code: Appends
    /// </summary>
    public const string LiteralAppends = "appends";

    /// <summary>
    /// Literal for code: ReplacedWith
    /// </summary>
    public const string LiteralReplacedWith = "replacedWith";

    /// <summary>
    /// Literal for code: Replaces
    /// </summary>
    public const string LiteralReplaces = "replaces";

    /// <summary>
    /// Literal for code: TransformedWith
    /// </summary>
    public const string LiteralTransformedWith = "transformedWith";

    /// <summary>
    /// Literal for code: Transforms
    /// </summary>
    public const string LiteralTransforms = "transforms";
  };
}
