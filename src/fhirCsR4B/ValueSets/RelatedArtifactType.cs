// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// The type of relationship to the related artifact.
  /// </summary>
  public static class RelatedArtifactTypeCodes
  {
    /// <summary>
    /// Bibliographic citation for papers, references, or other relevant material for the knowledge resource. This is intended to allow for citation of related material, but that was not necessarily specifically prepared in connection with this knowledge resource.
    /// </summary>
    public static readonly Coding Citation = new Coding
    {
      Code = "citation",
      Display = "Citation",
      System = "http://hl7.org/fhir/related-artifact-type"
    };
    /// <summary>
    /// The knowledge resource is composed of the given related artifact.
    /// </summary>
    public static readonly Coding ComposedOf = new Coding
    {
      Code = "composed-of",
      Display = "Composed Of",
      System = "http://hl7.org/fhir/related-artifact-type"
    };
    /// <summary>
    /// The knowledge resource depends on the given related artifact.
    /// </summary>
    public static readonly Coding DependsOn = new Coding
    {
      Code = "depends-on",
      Display = "Depends On",
      System = "http://hl7.org/fhir/related-artifact-type"
    };
    /// <summary>
    /// The knowledge resource is derived from the related artifact. This is intended to capture the relationship in which a particular knowledge resource is based on the content of another artifact, but is modified to capture either a different set of overall requirements, or a more specific set of requirements such as those involved in a particular institution or clinical setting.
    /// </summary>
    public static readonly Coding DerivedFrom = new Coding
    {
      Code = "derived-from",
      Display = "Derived From",
      System = "http://hl7.org/fhir/related-artifact-type"
    };
    /// <summary>
    /// Additional documentation for the knowledge resource. This would include additional instructions on usage as well as additional information on clinical context or appropriateness.
    /// </summary>
    public static readonly Coding Documentation = new Coding
    {
      Code = "documentation",
      Display = "Documentation",
      System = "http://hl7.org/fhir/related-artifact-type"
    };
    /// <summary>
    /// A summary of the justification for the knowledge resource including supporting evidence, relevant guidelines, or other clinically important information. This information is intended to provide a way to make the justification for the knowledge resource available to the consumer of interventions or results produced by the knowledge resource.
    /// </summary>
    public static readonly Coding Justification = new Coding
    {
      Code = "justification",
      Display = "Justification",
      System = "http://hl7.org/fhir/related-artifact-type"
    };
    /// <summary>
    /// The previous version of the knowledge resource.
    /// </summary>
    public static readonly Coding Predecessor = new Coding
    {
      Code = "predecessor",
      Display = "Predecessor",
      System = "http://hl7.org/fhir/related-artifact-type"
    };
    /// <summary>
    /// The next version of the knowledge resource.
    /// </summary>
    public static readonly Coding Successor = new Coding
    {
      Code = "successor",
      Display = "Successor",
      System = "http://hl7.org/fhir/related-artifact-type"
    };

    /// <summary>
    /// Literal for code: Citation
    /// </summary>
    public const string LiteralCitation = "citation";

    /// <summary>
    /// Literal for code: ComposedOf
    /// </summary>
    public const string LiteralComposedOf = "composed-of";

    /// <summary>
    /// Literal for code: DependsOn
    /// </summary>
    public const string LiteralDependsOn = "depends-on";

    /// <summary>
    /// Literal for code: DerivedFrom
    /// </summary>
    public const string LiteralDerivedFrom = "derived-from";

    /// <summary>
    /// Literal for code: Documentation
    /// </summary>
    public const string LiteralDocumentation = "documentation";

    /// <summary>
    /// Literal for code: Justification
    /// </summary>
    public const string LiteralJustification = "justification";

    /// <summary>
    /// Literal for code: Predecessor
    /// </summary>
    public const string LiteralPredecessor = "predecessor";

    /// <summary>
    /// Literal for code: Successor
    /// </summary>
    public const string LiteralSuccessor = "successor";
  };
}
