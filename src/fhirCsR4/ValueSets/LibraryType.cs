// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// The type of knowledge asset this library contains.
  /// </summary>
  public static class LibraryTypeCodes
  {
    /// <summary>
    /// The resource is a collection of knowledge assets.
    /// </summary>
    public static readonly Coding AssetCollection = new Coding
    {
      Code = "asset-collection",
      Display = "Asset Collection",
      System = "http://terminology.hl7.org/CodeSystem/library-type"
    };
    /// <summary>
    /// The resource is a shareable library of formalized knowledge.
    /// </summary>
    public static readonly Coding LogicLibrary = new Coding
    {
      Code = "logic-library",
      Display = "Logic Library",
      System = "http://terminology.hl7.org/CodeSystem/library-type"
    };
    /// <summary>
    /// The resource is a definition of an information model.
    /// </summary>
    public static readonly Coding ModelDefinition = new Coding
    {
      Code = "model-definition",
      Display = "Model Definition",
      System = "http://terminology.hl7.org/CodeSystem/library-type"
    };
    /// <summary>
    /// The resource defines the dependencies, parameters, and data requirements for a particular module or evaluation context.
    /// </summary>
    public static readonly Coding ModuleDefinition = new Coding
    {
      Code = "module-definition",
      Display = "Module Definition",
      System = "http://terminology.hl7.org/CodeSystem/library-type"
    };

    /// <summary>
    /// Literal for code: AssetCollection
    /// </summary>
    public const string LiteralAssetCollection = "asset-collection";

    /// <summary>
    /// Literal for code: LogicLibrary
    /// </summary>
    public const string LiteralLogicLibrary = "logic-library";

    /// <summary>
    /// Literal for code: ModelDefinition
    /// </summary>
    public const string LiteralModelDefinition = "model-definition";

    /// <summary>
    /// Literal for code: ModuleDefinition
    /// </summary>
    public const string LiteralModuleDefinition = "module-definition";
  };
}
