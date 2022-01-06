// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// This value set includes a smattering of Service Place codes.
  /// </summary>
  public static class ServicePlaceCodes
  {
    /// <summary>
    /// A facility or location where drugs and other medically related items and services are sold, dispensed, or otherwise provided directly to patients.
    /// </summary>
    public static readonly Coding Pharmacy = new Coding
    {
      Code = "01",
      Display = "Pharmacy",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// A facility whose primary purpose is education.
    /// </summary>
    public static readonly Coding School = new Coding
    {
      Code = "03",
      Display = "School",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// A facility or location whose primary purpose is to provide temporary housing to homeless individuals (e.g., emergency shelters, individual or family shelters).
    /// </summary>
    public static readonly Coding HomelessShelter = new Coding
    {
      Code = "04",
      Display = "Homeless Shelter",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// A facility or location, owned and operated by the Indian Health Service, which provides diagnostic, therapeutic (surgical and nonsurgical), and rehabilitation services to American Indians and Alaska Natives who do not require hospitalization.
    /// </summary>
    public static readonly Coding IndianHealthServiceFreeStandingFacility = new Coding
    {
      Code = "05",
      Display = "Indian Health Service Free-standing Facility",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// A facility or location, owned and operated by the Indian Health Service, which provides diagnostic, therapeutic (surgical and nonsurgical), and rehabilitation services rendered by, or under the supervision of, physicians to American Indians and Alaska Natives admitted as inpatients or outpatients.
    /// </summary>
    public static readonly Coding IndianHealthServiceProviderBasedFacility = new Coding
    {
      Code = "06",
      Display = "Indian Health Service Provider-based Facility",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// A facility or location owned and operated by a federally recognized American Indian or Alaska Native tribe or tribal organization under a 638 agreement, which provides diagnostic, therapeutic (surgical and nonsurgical), and rehabilitation services to tribal members who do not require hospitalization.
    /// </summary>
    public static readonly Coding Tribal638FreeStandingFacility = new Coding
    {
      Code = "07",
      Display = "Tribal 638 Free-Standing Facility",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// A facility or location owned and operated by a federally recognized American Indian or Alaska Native tribe or tribal organization under a 638 agreement, which provides diagnostic, therapeutic (surgical and nonsurgical), and rehabilitation services to tribal members admitted as inpatients or outpatients.
    /// </summary>
    public static readonly Coding Tribal638ProviderBasedFacility = new Coding
    {
      Code = "08",
      Display = "Tribal 638 Provider-Based Facility",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// A prison, jail, reformatory, work farm, detention center, or any other similar facility maintained by either Federal, State or local authorities for the purpose of confinement or rehabilitation of adult or juvenile criminal offenders.
    /// </summary>
    public static readonly Coding PrisonCorrectionalFacility = new Coding
    {
      Code = "09",
      Display = "Prison/Correctional Facility",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// Location, other than a hospital, skilled nursing facility (SNF), military treatment facility, community health center, State or local public health clinic, or intermediate care facility (ICF), where the health professional routinely provides health examinations, diagnosis, and treatment of illness or injury on an ambulatory basis.
    /// </summary>
    public static readonly Coding Office = new Coding
    {
      Code = "11",
      Display = "Office",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// Location, other than a hospital or other facility, where the patient receives care in a private residence.
    /// </summary>
    public static readonly Coding Home = new Coding
    {
      Code = "12",
      Display = "Home",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// Congregate residential facility with self-contained living units providing assessment of each resident's needs and on-site support 24 hours a day, 7 days a week, with the capacity to deliver or arrange for services including some health care and other services.
    /// </summary>
    public static readonly Coding AssistedLivingFa = new Coding
    {
      Code = "13",
      Display = "Assisted Living Fa",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// A residence, with shared living areas, where clients receive supervision and other services such as social and/or behavioral services, custodial service, and minimal services (e.g., medication administration).
    /// </summary>
    public static readonly Coding GroupHome = new Coding
    {
      Code = "14",
      Display = "Group Home",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// A facility/unit that moves from place-to-place equipped to provide preventive, screening, diagnostic, and/or treatment services.
    /// </summary>
    public static readonly Coding MobileUnit = new Coding
    {
      Code = "15",
      Display = "Mobile Unit",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// portion of an off-campus hospital provider based department which provides diagnostic, therapeutic (both surgical and nonsurgical), and rehabilitation services to sick or injured persons who do not require hospitalization or institutionalization.
    /// </summary>
    public static readonly Coding OffCampusOutpatientHospital = new Coding
    {
      Code = "19",
      Display = "Off Campus-Outpatient Hospital",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// Location, distinct from a hospital emergency room, an office, or a clinic, whose purpose is to diagnose and treat illness or injury for unscheduled, ambulatory patients seeking immediate medical attention.
    /// </summary>
    public static readonly Coding UrgentCareFacility = new Coding
    {
      Code = "20",
      Display = "Urgent Care Facility",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// A facility, other than psychiatric, which primarily provides diagnostic, therapeutic (both surgical and nonsurgical), and rehabilitation services by, or under, the supervision of physicians to patients admitted for a variety of medical conditions.
    /// </summary>
    public static readonly Coding InpatientHospital = new Coding
    {
      Code = "21",
      Display = "Inpatient Hospital",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };
    /// <summary>
    /// A land vehicle specifically designed, equipped and staffed for lifesaving and transporting the sick or injured.
    /// </summary>
    public static readonly Coding AmbulanceLand = new Coding
    {
      Code = "41",
      Display = "Ambulance—Land",
      System = "http://hl7.org/fhir/ex-serviceplace"
    };

    /// <summary>
    /// Literal for code: Pharmacy
    /// </summary>
    public const string LiteralPharmacy = "01";

    /// <summary>
    /// Literal for code: School
    /// </summary>
    public const string LiteralSchool = "03";

    /// <summary>
    /// Literal for code: HomelessShelter
    /// </summary>
    public const string LiteralHomelessShelter = "04";

    /// <summary>
    /// Literal for code: IndianHealthServiceFreeStandingFacility
    /// </summary>
    public const string LiteralIndianHealthServiceFreeStandingFacility = "05";

    /// <summary>
    /// Literal for code: IndianHealthServiceProviderBasedFacility
    /// </summary>
    public const string LiteralIndianHealthServiceProviderBasedFacility = "06";

    /// <summary>
    /// Literal for code: Tribal638FreeStandingFacility
    /// </summary>
    public const string LiteralTribal638FreeStandingFacility = "07";

    /// <summary>
    /// Literal for code: Tribal638ProviderBasedFacility
    /// </summary>
    public const string LiteralTribal638ProviderBasedFacility = "08";

    /// <summary>
    /// Literal for code: PrisonCorrectionalFacility
    /// </summary>
    public const string LiteralPrisonCorrectionalFacility = "09";

    /// <summary>
    /// Literal for code: Office
    /// </summary>
    public const string LiteralOffice = "11";

    /// <summary>
    /// Literal for code: Home
    /// </summary>
    public const string LiteralHome = "12";

    /// <summary>
    /// Literal for code: AssistedLivingFa
    /// </summary>
    public const string LiteralAssistedLivingFa = "13";

    /// <summary>
    /// Literal for code: GroupHome
    /// </summary>
    public const string LiteralGroupHome = "14";

    /// <summary>
    /// Literal for code: MobileUnit
    /// </summary>
    public const string LiteralMobileUnit = "15";

    /// <summary>
    /// Literal for code: OffCampusOutpatientHospital
    /// </summary>
    public const string LiteralOffCampusOutpatientHospital = "19";

    /// <summary>
    /// Literal for code: UrgentCareFacility
    /// </summary>
    public const string LiteralUrgentCareFacility = "20";

    /// <summary>
    /// Literal for code: InpatientHospital
    /// </summary>
    public const string LiteralInpatientHospital = "21";

    /// <summary>
    /// Literal for code: AmbulanceLand
    /// </summary>
    public const string LiteralAmbulanceLand = "41";
  };
}
