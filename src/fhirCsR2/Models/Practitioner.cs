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
  /// The list of roles/organizations that the practitioner is associated with.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<PractitionerPractitionerRole>))]
  public class PractitionerPractitionerRole : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The list of healthcare services that this worker provides for this role's Organization/Location(s).
    /// </summary>
    public List<Reference> HealthcareService { get; set; }
    /// <summary>
    /// The location(s) at which this practitioner provides care.
    /// </summary>
    public List<Reference> Location { get; set; }
    /// <summary>
    /// The organization where the Practitioner performs the roles associated.
    /// </summary>
    public Reference ManagingOrganization { get; set; }
    /// <summary>
    /// Even after the agencies is revoked, the fact that it existed must still be recorded.
    /// </summary>
    public Period Period { get; set; }
    /// <summary>
    /// Need to know what authority the practitioner has - what can they do?
    /// </summary>
    public CodeableConcept Role { get; set; }
    /// <summary>
    /// Specific specialty of the practitioner.
    /// </summary>
    public List<CodeableConcept> Specialty { get; set; }
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

      if (ManagingOrganization != null)
      {
        writer.WritePropertyName("managingOrganization");
        ManagingOrganization.SerializeJson(writer, options);
      }

      if (Role != null)
      {
        writer.WritePropertyName("role");
        Role.SerializeJson(writer, options);
      }

      if ((Specialty != null) && (Specialty.Count != 0))
      {
        writer.WritePropertyName("specialty");
        writer.WriteStartArray();

        foreach (CodeableConcept valSpecialty in Specialty)
        {
          valSpecialty.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
      }

      if ((Location != null) && (Location.Count != 0))
      {
        writer.WritePropertyName("location");
        writer.WriteStartArray();

        foreach (Reference valLocation in Location)
        {
          valLocation.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((HealthcareService != null) && (HealthcareService.Count != 0))
      {
        writer.WritePropertyName("healthcareService");
        writer.WriteStartArray();

        foreach (Reference valHealthcareService in HealthcareService)
        {
          valHealthcareService.SerializeJson(writer, options, true);
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
        case "healthcareService":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          HealthcareService = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.Reference objHealthcareService = new fhirCsR2.Models.Reference();
            objHealthcareService.DeserializeJson(ref reader, options);
            HealthcareService.Add(objHealthcareService);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (HealthcareService.Count == 0)
          {
            HealthcareService = null;
          }

          break;

        case "location":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Location = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.Reference objLocation = new fhirCsR2.Models.Reference();
            objLocation.DeserializeJson(ref reader, options);
            Location.Add(objLocation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Location.Count == 0)
          {
            Location = null;
          }

          break;

        case "managingOrganization":
          ManagingOrganization = new fhirCsR2.Models.Reference();
          ManagingOrganization.DeserializeJson(ref reader, options);
          break;

        case "period":
          Period = new fhirCsR2.Models.Period();
          Period.DeserializeJson(ref reader, options);
          break;

        case "role":
          Role = new fhirCsR2.Models.CodeableConcept();
          Role.DeserializeJson(ref reader, options);
          break;

        case "specialty":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Specialty = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.CodeableConcept objSpecialty = new fhirCsR2.Models.CodeableConcept();
            objSpecialty.DeserializeJson(ref reader, options);
            Specialty.Add(objSpecialty);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Specialty.Count == 0)
          {
            Specialty = null;
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
  /// Qualifications obtained by training and certification.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<PractitionerQualification>))]
  public class PractitionerQualification : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Coded representation of the qualification.
    /// </summary>
    public CodeableConcept Code { get; set; }
    /// <summary>
    /// Often, specific identities are assigned for the qualification.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// Organization that regulates and issues the qualification.
    /// </summary>
    public Reference Issuer { get; set; }
    /// <summary>
    /// Qualifications are often for a limited period of time, and can be revoked.
    /// </summary>
    public Period Period { get; set; }
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

      if (Code != null)
      {
        writer.WritePropertyName("code");
        Code.SerializeJson(writer, options);
      }

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
      }

      if (Issuer != null)
      {
        writer.WritePropertyName("issuer");
        Issuer.SerializeJson(writer, options);
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
        case "code":
          Code = new fhirCsR2.Models.CodeableConcept();
          Code.DeserializeJson(ref reader, options);
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

        case "issuer":
          Issuer = new fhirCsR2.Models.Reference();
          Issuer.DeserializeJson(ref reader, options);
          break;

        case "period":
          Period = new fhirCsR2.Models.Period();
          Period.DeserializeJson(ref reader, options);
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
  /// A person who is directly or indirectly involved in the provisioning of healthcare.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<Practitioner>))]
  public class Practitioner : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "Practitioner";
    /// <summary>
    /// Need to be able to mark a practitioner record as not to be used because it was created in error.
    /// </summary>
    public bool? Active { get; set; }
    /// <summary>
    /// Extension container element for Active
    /// </summary>
    public Element _Active { get; set; }
    /// <summary>
    /// Need to keep track where the practitioner can found during work or for directing mail.
    /// </summary>
    public List<Address> Address { get; set; }
    /// <summary>
    /// Needed for identification.
    /// </summary>
    public string BirthDate { get; set; }
    /// <summary>
    /// Extension container element for BirthDate
    /// </summary>
    public Element _BirthDate { get; set; }
    /// <summary>
    /// Knowing which language a practitioner speaks can help in facilitating communication with patients.
    /// </summary>
    public List<CodeableConcept> Communication { get; set; }
    /// <summary>
    /// Needed to address the person correctly.
    /// </summary>
    public string Gender { get; set; }
    /// <summary>
    /// Extension container element for Gender
    /// </summary>
    public Element _Gender { get; set; }
    /// <summary>
    /// Often, specific identities are assigned for the agent.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// Contact persons need to be identified by name, but it is uncommon to need details about multiple other names for that person.
    /// </summary>
    public HumanName Name { get; set; }
    /// <summary>
    /// Many EHR systems have the capability to capture an image of patients and personnel. Fits with newer social media usage too.
    /// </summary>
    public List<Attachment> Photo { get; set; }
    /// <summary>
    /// The list of roles/organizations that the practitioner is associated with.
    /// </summary>
    public List<PractitionerPractitionerRole> PractitionerRole { get; set; }
    /// <summary>
    /// Qualifications obtained by training and certification.
    /// </summary>
    public List<PractitionerQualification> Qualification { get; set; }
    /// <summary>
    /// Need to know how to reach a practitioner.
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

      if ((PractitionerRole != null) && (PractitionerRole.Count != 0))
      {
        writer.WritePropertyName("practitionerRole");
        writer.WriteStartArray();

        foreach (PractitionerPractitionerRole valPractitionerRole in PractitionerRole)
        {
          valPractitionerRole.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Qualification != null) && (Qualification.Count != 0))
      {
        writer.WritePropertyName("qualification");
        writer.WriteStartArray();

        foreach (PractitionerQualification valQualification in Qualification)
        {
          valQualification.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Communication != null) && (Communication.Count != 0))
      {
        writer.WritePropertyName("communication");
        writer.WriteStartArray();

        foreach (CodeableConcept valCommunication in Communication)
        {
          valCommunication.SerializeJson(writer, options, true);
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

        case "birthDate":
          BirthDate = reader.GetString();
          break;

        case "_birthDate":
          _BirthDate = new fhirCsR2.Models.Element();
          _BirthDate.DeserializeJson(ref reader, options);
          break;

        case "communication":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Communication = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.CodeableConcept objCommunication = new fhirCsR2.Models.CodeableConcept();
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

        case "name":
          Name = new fhirCsR2.Models.HumanName();
          Name.DeserializeJson(ref reader, options);
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

        case "practitionerRole":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          PractitionerRole = new List<PractitionerPractitionerRole>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.PractitionerPractitionerRole objPractitionerRole = new fhirCsR2.Models.PractitionerPractitionerRole();
            objPractitionerRole.DeserializeJson(ref reader, options);
            PractitionerRole.Add(objPractitionerRole);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (PractitionerRole.Count == 0)
          {
            PractitionerRole = null;
          }

          break;

        case "qualification":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Qualification = new List<PractitionerQualification>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.PractitionerQualification objQualification = new fhirCsR2.Models.PractitionerQualification();
            objQualification.DeserializeJson(ref reader, options);
            Qualification.Add(objQualification);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Qualification.Count == 0)
          {
            Qualification = null;
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
  /// Code Values for the Practitioner.gender field
  /// </summary>
  public static class PractitionerGenderCodes {
    public const string MALE = "male";
    public const string FEMALE = "female";
    public const string OTHER = "other";
    public const string UNKNOWN = "unknown";
  }
}
