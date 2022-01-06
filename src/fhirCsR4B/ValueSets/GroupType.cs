// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// Types of resources that are part of group.
  /// </summary>
  public static class GroupTypeCodes
  {
    /// <summary>
    /// Group contains "animal" Patient resources.
    /// </summary>
    public static readonly Coding Animal = new Coding
    {
      Code = "animal",
      Display = "Animal",
      System = "http://hl7.org/fhir/group-type"
    };
    /// <summary>
    /// Group contains Device resources.
    /// </summary>
    public static readonly Coding Device = new Coding
    {
      Code = "device",
      Display = "Device",
      System = "http://hl7.org/fhir/group-type"
    };
    /// <summary>
    /// Group contains Medication resources.
    /// </summary>
    public static readonly Coding Medication = new Coding
    {
      Code = "medication",
      Display = "Medication",
      System = "http://hl7.org/fhir/group-type"
    };
    /// <summary>
    /// Group contains "person" Patient resources.
    /// </summary>
    public static readonly Coding Person = new Coding
    {
      Code = "person",
      Display = "Person",
      System = "http://hl7.org/fhir/group-type"
    };
    /// <summary>
    /// Group contains healthcare practitioner resources (Practitioner or PractitionerRole).
    /// </summary>
    public static readonly Coding Practitioner = new Coding
    {
      Code = "practitioner",
      Display = "Practitioner",
      System = "http://hl7.org/fhir/group-type"
    };
    /// <summary>
    /// Group contains Substance resources.
    /// </summary>
    public static readonly Coding Substance = new Coding
    {
      Code = "substance",
      Display = "Substance",
      System = "http://hl7.org/fhir/group-type"
    };

    /// <summary>
    /// Literal for code: Animal
    /// </summary>
    public const string LiteralAnimal = "animal";

    /// <summary>
    /// Literal for code: Device
    /// </summary>
    public const string LiteralDevice = "device";

    /// <summary>
    /// Literal for code: Medication
    /// </summary>
    public const string LiteralMedication = "medication";

    /// <summary>
    /// Literal for code: Person
    /// </summary>
    public const string LiteralPerson = "person";

    /// <summary>
    /// Literal for code: Practitioner
    /// </summary>
    public const string LiteralPractitioner = "practitioner";

    /// <summary>
    /// Literal for code: Substance
    /// </summary>
    public const string LiteralSubstance = "substance";
  };
}
