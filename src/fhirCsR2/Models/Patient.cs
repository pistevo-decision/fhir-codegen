// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR2.Serialization;

namespace fhirCsR2.Models
{
  /// <summary>
  /// Need to track people you can contact about the patient.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<PatientContact>))]
  public class PatientContact : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Need to keep track where the contact person can be contacted per postal mail or visited.
    /// </summary>
    public Address Address { get; set; }
    /// <summary>
    /// Needed to address the person correctly.
    /// </summary>
    public string Gender { get; set; }
    /// <summary>
    /// Extension container element for Gender
    /// </summary>
    public Element _Gender { get; set; }
    /// <summary>
    /// Contact persons need to be identified by name, but it is uncommon to need details about multiple other names for that contact person.
    /// </summary>
    public HumanName Name { get; set; }
    /// <summary>
    /// For guardians or business related contacts, the organization is relevant.
    /// </summary>
    public Reference Organization { get; set; }
    /// <summary>
    /// The period during which this contact person or organization is valid to be contacted relating to this patient.
    /// </summary>
    public Period Period { get; set; }
    /// <summary>
    /// Used to determine which contact person is the most relevant to approach, depending on circumstances.
    /// </summary>
    public List<CodeableConcept> Relationship { get; set; }
    /// <summary>
    /// People have (primary) ways to contact them in some way such as phone, email.
    /// </summary>
    public List<ContactPoint> Telecom { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR2.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if ((Relationship != null) && (Relationship.Count != 0))
      {
        writer.WritePropertyName("relationship");
        writer.WriteStartArray();

        foreach (CodeableConcept valRelationship in Relationship)
        {
          valRelationship.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Name != null)
      {
        writer.WritePropertyName("name");
        Name.SerializeJson(writer, options);
      }

      if ((Telecom != null) && (Telecom.Count != 0))
      {
        writer.WritePropertyName("telecom");
        writer.WriteStartArray();

        foreach (ContactPoint valTelecom in Telecom)
        {
          valTelecom.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Address != null)
      {
        writer.WritePropertyName("address");
        Address.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Gender))
      {
        writer.WriteString("gender", (string)Gender!);
      }

      if (_Gender != null)
      {
        writer.WritePropertyName("_gender");
        _Gender.SerializeJson(writer, options);
      }

      if (Organization != null)
      {
        writer.WritePropertyName("organization");
        Organization.SerializeJson(writer, options);
      }

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "address":
          Address = new fhirCsR2.Models.Address();
          Address.DeserializeJson(ref reader, options);
          break;

        case "gender":
          Gender = reader.GetString();
          break;

        case "_gender":
          _Gender = new fhirCsR2.Models.Element();
          _Gender.DeserializeJson(ref reader, options);
          break;

        case "name":
          Name = new fhirCsR2.Models.HumanName();
          Name.DeserializeJson(ref reader, options);
          break;

        case "organization":
          Organization = new fhirCsR2.Models.Reference();
          Organization.DeserializeJson(ref reader, options);
          break;

        case "period":
          Period = new fhirCsR2.Models.Period();
          Period.DeserializeJson(ref reader, options);
          break;

        case "relationship":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Relationship = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.CodeableConcept objRelationship = new fhirCsR2.Models.CodeableConcept();
            objRelationship.DeserializeJson(ref reader, options);
            Relationship.Add(objRelationship);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Relationship.Count == 0)
          {
            Relationship = null;
          }

          break;

        case "telecom":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Telecom = new List<ContactPoint>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.ContactPoint objTelecom = new fhirCsR2.Models.ContactPoint();
            objTelecom.DeserializeJson(ref reader, options);
            Telecom.Add(objTelecom);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Telecom.Count == 0)
          {
            Telecom = null;
          }

          break;

        default:
          ((fhirCsR2.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// Code Values for the Patient.contact.gender field
  /// </summary>
  public static class PatientContactGenderCodes {
    public const string MALE = "male";
    public const string FEMALE = "female";
    public const string OTHER = "other";
    public const string UNKNOWN = "unknown";
  }
  /// <summary>
  /// Many clinical systems are extended to care for animal patients as well as human.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<PatientAnimal>))]
  public class PatientAnimal : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// May need to know the specific kind within the species.
    /// </summary>
    public CodeableConcept Breed { get; set; }
    /// <summary>
    /// Gender status can affect housing and animal behavior.
    /// </summary>
    public CodeableConcept GenderStatus { get; set; }
    /// <summary>
    /// Need to know what kind of animal.
    /// </summary>
    public CodeableConcept Species { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR2.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Species != null)
      {
        writer.WritePropertyName("species");
        Species.SerializeJson(writer, options);
      }

      if (Breed != null)
      {
        writer.WritePropertyName("breed");
        Breed.SerializeJson(writer, options);
      }

      if (GenderStatus != null)
      {
        writer.WritePropertyName("genderStatus");
        GenderStatus.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "breed":
          Breed = new fhirCsR2.Models.CodeableConcept();
          Breed.DeserializeJson(ref reader, options);
          break;

        case "genderStatus":
          GenderStatus = new fhirCsR2.Models.CodeableConcept();
          GenderStatus.DeserializeJson(ref reader, options);
          break;

        case "species":
          Species = new fhirCsR2.Models.CodeableConcept();
          Species.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR2.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// If a patient does not speak the local language, interpreters may be required, so languages spoken and proficiency is an important things to keep track of both for patient and other persons of interest.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<PatientCommunication>))]
  public class PatientCommunication : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Most systems in multilingual countries will want to convey language. Not all systems actually need the regional dialect.
    /// </summary>
    public CodeableConcept Language { get; set; }
    /// <summary>
    /// People that master multiple languages up to certain level may prefer one or more, i.e. feel more confident in communicating in a particular language making other languages sort of a fall back method.
    /// </summary>
    public bool? Preferred { get; set; }
    /// <summary>
    /// Extension container element for Preferred
    /// </summary>
    public Element _Preferred { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR2.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Language != null)
      {
        writer.WritePropertyName("language");
        Language.SerializeJson(writer, options);
      }

      if (Preferred != null)
      {
        writer.WriteBoolean("preferred", (bool)Preferred!);
      }

      if (_Preferred != null)
      {
        writer.WritePropertyName("_preferred");
        _Preferred.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "language":
          Language = new fhirCsR2.Models.CodeableConcept();
          Language.DeserializeJson(ref reader, options);
          break;

        case "preferred":
          Preferred = reader.GetBoolean();
          break;

        case "_preferred":
          _Preferred = new fhirCsR2.Models.Element();
          _Preferred.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR2.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// There are multiple usecases:   * Duplicate patient records due to the clerical errors associated with the difficulties of identifying humans consistently, and * Distribution of patient information across multiple servers.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<PatientLink>))]
  public class PatientLink : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The other patient resource that the link refers to.
    /// </summary>
    public Reference Other { get; set; }
    /// <summary>
    /// The type of link between this patient resource and another patient resource.
    /// </summary>
    public string Type { get; set; }
    /// <summary>
    /// Extension container element for Type
    /// </summary>
    public Element _Type { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR2.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Other != null)
      {
        writer.WritePropertyName("other");
        Other.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Type))
      {
        writer.WriteString("type", (string)Type!);
      }

      if (_Type != null)
      {
        writer.WritePropertyName("_type");
        _Type.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "other":
          Other = new fhirCsR2.Models.Reference();
          Other.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = reader.GetString();
          break;

        case "_type":
          _Type = new fhirCsR2.Models.Element();
          _Type.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR2.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// Code Values for the Patient.link.type field
  /// </summary>
  public static class PatientLinkTypeCodes {
    public const string REPLACE = "replace";
    public const string REFER = "refer";
    public const string SEEALSO = "seealso";
  }
  /// <summary>
  /// Demographics and other administrative information about an individual or animal receiving care or other health-related services.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<Patient>))]
  public class Patient : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "Patient";
    /// <summary>
    /// Need to be able to mark a patient record as not to be used because it was created in error.
    /// </summary>
    public bool? Active { get; set; }
    /// <summary>
    /// Extension container element for Active
    /// </summary>
    public Element _Active { get; set; }
    /// <summary>
    /// May need to keep track of patient addresses for contacting, billing or reporting requirements and also to help with identification.
    /// </summary>
    public List<Address> Address { get; set; }
    /// <summary>
    /// Many clinical systems are extended to care for animal patients as well as human.
    /// </summary>
    public PatientAnimal Animal { get; set; }
    /// <summary>
    /// Age of the individual drives many clinical processes.
    /// </summary>
    public string BirthDate { get; set; }
    /// <summary>
    /// Extension container element for BirthDate
    /// </summary>
    public Element _BirthDate { get; set; }
    /// <summary>
    /// Patient's nominated care provider.
    /// </summary>
    public List<Reference> CareProvider { get; set; }
    /// <summary>
    /// If a patient does not speak the local language, interpreters may be required, so languages spoken and proficiency is an important things to keep track of both for patient and other persons of interest.
    /// </summary>
    public List<PatientCommunication> Communication { get; set; }
    /// <summary>
    /// Need to track people you can contact about the patient.
    /// </summary>
    public List<PatientContact> Contact { get; set; }
    /// <summary>
    /// The fact that a patient is deceased influences the clinical process. Also, in human communication and relation management it is necessary to know whether the person is alive.
    /// </summary>
    public bool? DeceasedBoolean { get; set; }
    /// <summary>
    /// Extension container element for DeceasedBoolean
    /// </summary>
    public Element _DeceasedBoolean { get; set; }
    /// <summary>
    /// The fact that a patient is deceased influences the clinical process. Also, in human communication and relation management it is necessary to know whether the person is alive.
    /// </summary>
    public string DeceasedDateTime { get; set; }
    /// <summary>
    /// Extension container element for DeceasedDateTime
    /// </summary>
    public Element _DeceasedDateTime { get; set; }
    /// <summary>
    /// Needed for identification of the individual, in combination with (at least) name and birth date. Gender of individual drives many clinical processes.
    /// </summary>
    public string Gender { get; set; }
    /// <summary>
    /// Extension container element for Gender
    /// </summary>
    public Element _Gender { get; set; }
    /// <summary>
    /// Patients are almost always assigned specific numerical identifiers.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// There are multiple usecases:   * Duplicate patient records due to the clerical errors associated with the difficulties of identifying humans consistently, and * Distribution of patient information across multiple servers.
    /// </summary>
    public List<PatientLink> Link { get; set; }
    /// <summary>
    /// Need to know who recognizes this patient record, manages and updates it.
    /// </summary>
    public Reference ManagingOrganization { get; set; }
    /// <summary>
    /// Most, if not all systems capture it.
    /// </summary>
    public CodeableConcept MaritalStatus { get; set; }
    /// <summary>
    /// For disambiguation of multiple-birth children, especially relevant where the care provider doesn't meet the patient, such as labs.
    /// </summary>
    public bool? MultipleBirthBoolean { get; set; }
    /// <summary>
    /// Extension container element for MultipleBirthBoolean
    /// </summary>
    public Element _MultipleBirthBoolean { get; set; }
    /// <summary>
    /// For disambiguation of multiple-birth children, especially relevant where the care provider doesn't meet the patient, such as labs.
    /// </summary>
    public int? MultipleBirthInteger { get; set; }
    /// <summary>
    /// Extension container element for MultipleBirthInteger
    /// </summary>
    public Element _MultipleBirthInteger { get; set; }
    /// <summary>
    /// Need to be able to track the patient by multiple names. Examples are your official name and a partner name.
    /// </summary>
    public List<HumanName> Name { get; set; }
    /// <summary>
    /// Many EHR systems have the capability to capture an image of the patient. Fits with newer social media usage too.
    /// </summary>
    public List<Attachment> Photo { get; set; }
    /// <summary>
    /// People have (primary) ways to contact them in some way such as phone, email.
    /// </summary>
    public List<ContactPoint> Telecom { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      if (!string.IsNullOrEmpty(ResourceType))
      {
        writer.WriteString("resourceType", (string)ResourceType!);
      }


      ((fhirCsR2.Models.DomainResource)this).SerializeJson(writer, options, false);

      if ((Identifier != null) && (Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();

        foreach (Identifier valIdentifier in Identifier)
        {
          valIdentifier.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Active != null)
      {
        writer.WriteBoolean("active", (bool)Active!);
      }

      if (_Active != null)
      {
        writer.WritePropertyName("_active");
        _Active.SerializeJson(writer, options);
      }

      if ((Name != null) && (Name.Count != 0))
      {
        writer.WritePropertyName("name");
        writer.WriteStartArray();

        foreach (HumanName valName in Name)
        {
          valName.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Telecom != null) && (Telecom.Count != 0))
      {
        writer.WritePropertyName("telecom");
        writer.WriteStartArray();

        foreach (ContactPoint valTelecom in Telecom)
        {
          valTelecom.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Gender))
      {
        writer.WriteString("gender", (string)Gender!);
      }

      if (_Gender != null)
      {
        writer.WritePropertyName("_gender");
        _Gender.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(BirthDate))
      {
        writer.WriteString("birthDate", (string)BirthDate!);
      }

      if (_BirthDate != null)
      {
        writer.WritePropertyName("_birthDate");
        _BirthDate.SerializeJson(writer, options);
      }

      if (DeceasedBoolean != null)
      {
        writer.WriteBoolean("deceasedBoolean", (bool)DeceasedBoolean!);
      }

      if (_DeceasedBoolean != null)
      {
        writer.WritePropertyName("_deceasedBoolean");
        _DeceasedBoolean.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(DeceasedDateTime))
      {
        writer.WriteString("deceasedDateTime", (string)DeceasedDateTime!);
      }

      if (_DeceasedDateTime != null)
      {
        writer.WritePropertyName("_deceasedDateTime");
        _DeceasedDateTime.SerializeJson(writer, options);
      }

      if ((Address != null) && (Address.Count != 0))
      {
        writer.WritePropertyName("address");
        writer.WriteStartArray();

        foreach (Address valAddress in Address)
        {
          valAddress.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (MaritalStatus != null)
      {
        writer.WritePropertyName("maritalStatus");
        MaritalStatus.SerializeJson(writer, options);
      }

      if (MultipleBirthBoolean != null)
      {
        writer.WriteBoolean("multipleBirthBoolean", (bool)MultipleBirthBoolean!);
      }

      if (_MultipleBirthBoolean != null)
      {
        writer.WritePropertyName("_multipleBirthBoolean");
        _MultipleBirthBoolean.SerializeJson(writer, options);
      }

      if (MultipleBirthInteger != null)
      {
        writer.WriteNumber("multipleBirthInteger", (int)MultipleBirthInteger!);
      }

      if (_MultipleBirthInteger != null)
      {
        writer.WritePropertyName("_multipleBirthInteger");
        _MultipleBirthInteger.SerializeJson(writer, options);
      }

      if ((Photo != null) && (Photo.Count != 0))
      {
        writer.WritePropertyName("photo");
        writer.WriteStartArray();

        foreach (Attachment valPhoto in Photo)
        {
          valPhoto.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Contact != null) && (Contact.Count != 0))
      {
        writer.WritePropertyName("contact");
        writer.WriteStartArray();

        foreach (PatientContact valContact in Contact)
        {
          valContact.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Animal != null)
      {
        writer.WritePropertyName("animal");
        Animal.SerializeJson(writer, options);
      }

      if ((Communication != null) && (Communication.Count != 0))
      {
        writer.WritePropertyName("communication");
        writer.WriteStartArray();

        foreach (PatientCommunication valCommunication in Communication)
        {
          valCommunication.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((CareProvider != null) && (CareProvider.Count != 0))
      {
        writer.WritePropertyName("careProvider");
        writer.WriteStartArray();

        foreach (Reference valCareProvider in CareProvider)
        {
          valCareProvider.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (ManagingOrganization != null)
      {
        writer.WritePropertyName("managingOrganization");
        ManagingOrganization.SerializeJson(writer, options);
      }

      if ((Link != null) && (Link.Count != 0))
      {
        writer.WritePropertyName("link");
        writer.WriteStartArray();

        foreach (PatientLink valLink in Link)
        {
          valLink.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "active":
          Active = reader.GetBoolean();
          break;

        case "_active":
          _Active = new fhirCsR2.Models.Element();
          _Active.DeserializeJson(ref reader, options);
          break;

        case "address":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Address = new List<Address>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.Address objAddress = new fhirCsR2.Models.Address();
            objAddress.DeserializeJson(ref reader, options);
            Address.Add(objAddress);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Address.Count == 0)
          {
            Address = null;
          }

          break;

        case "animal":
          Animal = new fhirCsR2.Models.PatientAnimal();
          Animal.DeserializeJson(ref reader, options);
          break;

        case "birthDate":
          BirthDate = reader.GetString();
          break;

        case "_birthDate":
          _BirthDate = new fhirCsR2.Models.Element();
          _BirthDate.DeserializeJson(ref reader, options);
          break;

        case "careProvider":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          CareProvider = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.Reference objCareProvider = new fhirCsR2.Models.Reference();
            objCareProvider.DeserializeJson(ref reader, options);
            CareProvider.Add(objCareProvider);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (CareProvider.Count == 0)
          {
            CareProvider = null;
          }

          break;

        case "communication":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Communication = new List<PatientCommunication>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.PatientCommunication objCommunication = new fhirCsR2.Models.PatientCommunication();
            objCommunication.DeserializeJson(ref reader, options);
            Communication.Add(objCommunication);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Communication.Count == 0)
          {
            Communication = null;
          }

          break;

        case "contact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Contact = new List<PatientContact>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.PatientContact objContact = new fhirCsR2.Models.PatientContact();
            objContact.DeserializeJson(ref reader, options);
            Contact.Add(objContact);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Contact.Count == 0)
          {
            Contact = null;
          }

          break;

        case "deceasedBoolean":
          DeceasedBoolean = reader.GetBoolean();
          break;

        case "_deceasedBoolean":
          _DeceasedBoolean = new fhirCsR2.Models.Element();
          _DeceasedBoolean.DeserializeJson(ref reader, options);
          break;

        case "deceasedDateTime":
          DeceasedDateTime = reader.GetString();
          break;

        case "_deceasedDateTime":
          _DeceasedDateTime = new fhirCsR2.Models.Element();
          _DeceasedDateTime.DeserializeJson(ref reader, options);
          break;

        case "gender":
          Gender = reader.GetString();
          break;

        case "_gender":
          _Gender = new fhirCsR2.Models.Element();
          _Gender.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.Identifier objIdentifier = new fhirCsR2.Models.Identifier();
            objIdentifier.DeserializeJson(ref reader, options);
            Identifier.Add(objIdentifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Identifier.Count == 0)
          {
            Identifier = null;
          }

          break;

        case "link":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Link = new List<PatientLink>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.PatientLink objLink = new fhirCsR2.Models.PatientLink();
            objLink.DeserializeJson(ref reader, options);
            Link.Add(objLink);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Link.Count == 0)
          {
            Link = null;
          }

          break;

        case "managingOrganization":
          ManagingOrganization = new fhirCsR2.Models.Reference();
          ManagingOrganization.DeserializeJson(ref reader, options);
          break;

        case "maritalStatus":
          MaritalStatus = new fhirCsR2.Models.CodeableConcept();
          MaritalStatus.DeserializeJson(ref reader, options);
          break;

        case "multipleBirthBoolean":
          MultipleBirthBoolean = reader.GetBoolean();
          break;

        case "_multipleBirthBoolean":
          _MultipleBirthBoolean = new fhirCsR2.Models.Element();
          _MultipleBirthBoolean.DeserializeJson(ref reader, options);
          break;

        case "multipleBirthInteger":
          MultipleBirthInteger = reader.GetInt32();
          break;

        case "_multipleBirthInteger":
          _MultipleBirthInteger = new fhirCsR2.Models.Element();
          _MultipleBirthInteger.DeserializeJson(ref reader, options);
          break;

        case "name":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Name = new List<HumanName>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.HumanName objName = new fhirCsR2.Models.HumanName();
            objName.DeserializeJson(ref reader, options);
            Name.Add(objName);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Name.Count == 0)
          {
            Name = null;
          }

          break;

        case "photo":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Photo = new List<Attachment>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.Attachment objPhoto = new fhirCsR2.Models.Attachment();
            objPhoto.DeserializeJson(ref reader, options);
            Photo.Add(objPhoto);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Photo.Count == 0)
          {
            Photo = null;
          }

          break;

        case "telecom":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Telecom = new List<ContactPoint>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.ContactPoint objTelecom = new fhirCsR2.Models.ContactPoint();
            objTelecom.DeserializeJson(ref reader, options);
            Telecom.Add(objTelecom);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Telecom.Count == 0)
          {
            Telecom = null;
          }

          break;

        default:
          ((fhirCsR2.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// Code Values for the Patient.gender field
  /// </summary>
  public static class PatientGenderCodes {
    public const string MALE = "male";
    public const string FEMALE = "female";
    public const string OTHER = "other";
    public const string UNKNOWN = "unknown";
  }
}
