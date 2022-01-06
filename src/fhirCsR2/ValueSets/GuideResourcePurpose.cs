// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// Why a resource is included in the guide.
  /// </summary>
  public static class GuideResourcePurposeCodes
  {
    /// <summary>
    /// The resource contains a dictionary that is part of the implementation guide.
    /// </summary>
    public static readonly Coding Dictionary = new Coding
    {
      Code = "dictionary",
      Display = "Dictionary",
      System = "http://hl7.org/fhir/guide-resource-purpose"
    };
    /// <summary>
    /// The resource is intended as an example.
    /// </summary>
    public static readonly Coding Example = new Coding
    {
      Code = "example",
      Display = "Example",
      System = "http://hl7.org/fhir/guide-resource-purpose"
    };
    /// <summary>
    /// The resource defines an extension (StructureDefinition) that is used in the implementation guide.
    /// </summary>
    public static readonly Coding Extension = new Coding
    {
      Code = "extension",
      Display = "Extension",
      System = "http://hl7.org/fhir/guide-resource-purpose"
    };
    /// <summary>
    /// The resource defines a logical model (in a StructureDefinition) that is used in the implementation guide.
    /// </summary>
    public static readonly Coding LogicalModel = new Coding
    {
      Code = "logical",
      Display = "Logical Model",
      System = "http://hl7.org/fhir/guide-resource-purpose"
    };
    /// <summary>
    /// The resource defines a profile (StructureDefinition) that is used in the implementation guide.
    /// </summary>
    public static readonly Coding Profile = new Coding
    {
      Code = "profile",
      Display = "Profile",
      System = "http://hl7.org/fhir/guide-resource-purpose"
    };
    /// <summary>
    /// The resource defines a value set or concept map used in the implementation guide.
    /// </summary>
    public static readonly Coding Terminology = new Coding
    {
      Code = "terminology",
      Display = "Terminology",
      System = "http://hl7.org/fhir/guide-resource-purpose"
    };

    /// <summary>
    /// Literal for code: Dictionary
    /// </summary>
    public const string LiteralDictionary = "dictionary";

    /// <summary>
    /// Literal for code: Example
    /// </summary>
    public const string LiteralExample = "example";

    /// <summary>
    /// Literal for code: Extension
    /// </summary>
    public const string LiteralExtension = "extension";

    /// <summary>
    /// Literal for code: LogicalModel
    /// </summary>
    public const string LiteralLogicalModel = "logical";

    /// <summary>
    /// Literal for code: Profile
    /// </summary>
    public const string LiteralProfile = "profile";

    /// <summary>
    /// Literal for code: Terminology
    /// </summary>
    public const string LiteralTerminology = "terminology";
  };
}
