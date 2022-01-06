// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Which compartment a compartment definition describes
  /// </summary>
  public static class CompartmentTypeCodes
  {
    /// <summary>
    /// The compartment definition is for the device compartment
    /// </summary>
    public static readonly Coding Device = new Coding
    {
      Code = "Device",
      Display = "Device",
      System = "http://hl7.org/fhir/compartment-type"
    };
    /// <summary>
    /// The compartment definition is for the encounter compartment
    /// </summary>
    public static readonly Coding Encounter = new Coding
    {
      Code = "Encounter",
      Display = "Encounter",
      System = "http://hl7.org/fhir/compartment-type"
    };
    /// <summary>
    /// The compartment definition is for the patient compartment
    /// </summary>
    public static readonly Coding Patient = new Coding
    {
      Code = "Patient",
      Display = "Patient",
      System = "http://hl7.org/fhir/compartment-type"
    };
    /// <summary>
    /// The compartment definition is for the practitioner compartment
    /// </summary>
    public static readonly Coding Practitioner = new Coding
    {
      Code = "Practitioner",
      Display = "Practitioner",
      System = "http://hl7.org/fhir/compartment-type"
    };
    /// <summary>
    /// The compartment definition is for the related-person compartment
    /// </summary>
    public static readonly Coding RelatedPerson = new Coding
    {
      Code = "RelatedPerson",
      Display = "RelatedPerson",
      System = "http://hl7.org/fhir/compartment-type"
    };

    /// <summary>
    /// Literal for code: Device
    /// </summary>
    public const string LiteralDevice = "Device";

    /// <summary>
    /// Literal for code: Encounter
    /// </summary>
    public const string LiteralEncounter = "Encounter";

    /// <summary>
    /// Literal for code: Patient
    /// </summary>
    public const string LiteralPatient = "Patient";

    /// <summary>
    /// Literal for code: Practitioner
    /// </summary>
    public const string LiteralPractitioner = "Practitioner";

    /// <summary>
    /// Literal for code: RelatedPerson
    /// </summary>
    public const string LiteralRelatedPerson = "RelatedPerson";
  };
}
