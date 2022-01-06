// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR5.Serialization;

namespace fhirCsR5.Models
{
  /// <summary>
  /// Multiple identifiers may exist, either due to duplicate registration, regional rules, needs of different communication technologies, etc.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<NamingSystemUniqueId>))]
  public class NamingSystemUniqueId : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Indicates whether this identifier ie endorsed by the official owner of the associated naming system.
    /// </summary>
    public bool? Authoritative { get; set; }
    /// <summary>
    /// e.g. "must be used in Germany" or "was initially published in error with this value".
    /// </summary>
    public string Comment { get; set; }
    /// <summary>
    /// Extension container element for Comment
    /// </summary>
    public Element _Comment { get; set; }
    /// <summary>
    /// Within a registry, a given identifier should only be "active" for a single namespace at a time.  (Ideally, an identifier should only ever be associated with a single namespace across all time).
    /// </summary>
    public Period Period { get; set; }
    /// <summary>
    /// Indicates whether this identifier is the "preferred" identifier of this type.
    /// </summary>
    public bool? Preferred { get; set; }
    /// <summary>
    /// Different identifier types may be used in different types of communications (OIDs for v3, URIs for FHIR, etc.).  Other includes RUIDs from v3, standard v2 code name strings, etc.
    /// </summary>
    public string Type { get; set; }
    /// <summary>
    /// Extension container element for Type
    /// </summary>
    public Element _Type { get; set; }
    /// <summary>
    /// If the value is a URI intended for use as FHIR system identifier, the URI should not contain "\" or "?" or "," since this makes escaping very difficult.
    /// </summary>
    public string Value { get; set; }
    /// <summary>
    /// Extension container element for Value
    /// </summary>
    public Element _Value { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Type))
      {
        writer.WriteString("type", (string)Type!);
      }

      if (_Type != null)
      {
        writer.WritePropertyName("_type");
        _Type.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Value))
      {
        writer.WriteString("value", (string)Value!);
      }

      if (_Value != null)
      {
        writer.WritePropertyName("_value");
        _Value.SerializeJson(writer, options);
      }

      if (Preferred != null)
      {
        writer.WriteBoolean("preferred", (bool)Preferred!);
      }

      if (!string.IsNullOrEmpty(Comment))
      {
        writer.WriteString("comment", (string)Comment!);
      }

      if (_Comment != null)
      {
        writer.WritePropertyName("_comment");
        _Comment.SerializeJson(writer, options);
      }

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
      }

      if (Authoritative != null)
      {
        writer.WriteBoolean("authoritative", (bool)Authoritative!);
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
        case "authoritative":
          Authoritative = reader.GetBoolean();
          break;

        case "comment":
          Comment = reader.GetString();
          break;

        case "_comment":
          _Comment = new fhirCsR5.Models.Element();
          _Comment.DeserializeJson(ref reader, options);
          break;

        case "period":
          Period = new fhirCsR5.Models.Period();
          Period.DeserializeJson(ref reader, options);
          break;

        case "preferred":
          Preferred = reader.GetBoolean();
          break;

        case "type":
          Type = reader.GetString();
          break;

        case "_type":
          _Type = new fhirCsR5.Models.Element();
          _Type.DeserializeJson(ref reader, options);
          break;

        case "value":
          Value = reader.GetString();
          break;

        case "_value":
          _Value = new fhirCsR5.Models.Element();
          _Value.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the NamingSystem.uniqueId.type field
  /// </summary>
  public static class NamingSystemUniqueIdTypeCodes {
    public const string OID = "oid";
    public const string UUID = "uuid";
    public const string URI = "uri";
    public const string V2CSMNEMONIC = "v2csmnemonic";
    public const string OTHER = "other";
  }
  /// <summary>
  /// A curated namespace that issues unique symbols within that namespace for the identification of concepts, people, devices, etc.  Represents a "System" used within the Identifier and Coding data types.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<NamingSystem>))]
  public class NamingSystem : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "NamingSystem";
    /// <summary>
    /// May be a web site, an email address, a telephone number, etc.
    /// </summary>
    public List<ContactDetail> Contact { get; set; }
    /// <summary>
    /// Note that this is not the same as the resource last-modified-date, since the resource may be a secondary representation of the naming system. Additional specific dates may be added as extensions or be found by consulting Provenances associated with past versions of the resource.
    /// </summary>
    public string Date { get; set; }
    /// <summary>
    /// Extension container element for Date
    /// </summary>
    public Element _Date { get; set; }
    /// <summary>
    /// This description can be used to capture details such as why the naming system was built, comments about misuse, instructions for clinical use and interpretation, literature references, examples from the paper world, etc. It is not a rendering of the naming system as conveyed in the 'text' field of the resource itself. This item SHOULD be populated unless the information is available from context (e.g. the language of the naming system is presumed to be the predominant language in the place the naming system was created).
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// It may be possible for the naming system to be used in jurisdictions other than those for which it was originally designed or intended.
    /// </summary>
    public List<CodeableConcept> Jurisdiction { get; set; }
    /// <summary>
    /// Indicates the purpose for the naming system - what kinds of things does it make unique?
    /// </summary>
    public string Kind { get; set; }
    /// <summary>
    /// Extension container element for Kind
    /// </summary>
    public Element _Kind { get; set; }
    /// <summary>
    /// The name is not expected to be globally unique. The name should be a simple alphanumeric type name to ensure that it is machine-processing friendly.The"symbolic name" for an OID would be captured as an extension.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// Usually an organization but may be an individual. The publisher (or steward) of the naming system is the organization or individual primarily responsible for the maintenance and upkeep of the naming system. This is not necessarily the same individual or organization that developed and initially authored the content. The publisher is the primary point of contact for questions or issues with the naming system. This item SHOULD be populated unless the information is available from context.
    /// </summary>
    public string Publisher { get; set; }
    /// <summary>
    /// Extension container element for Publisher
    /// </summary>
    public Element _Publisher { get; set; }
    /// <summary>
    /// This is the primary organization.  Responsibility for some aspects of a namespace may be delegated.
    /// </summary>
    public string Responsible { get; set; }
    /// <summary>
    /// Extension container element for Responsible
    /// </summary>
    public Element _Responsible { get; set; }
    /// <summary>
    /// Allows filtering of naming systems that are appropriate for use versus not.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// This title does not need to be machine-processing friendly and may contain punctuation, white-space, etc.
    /// </summary>
    public string Title { get; set; }
    /// <summary>
    /// Extension container element for Title
    /// </summary>
    public Element _Title { get; set; }
    /// <summary>
    /// This will most commonly be used for identifier namespaces, but categories could potentially be useful for code systems and authorities as well.
    /// </summary>
    public CodeableConcept Type { get; set; }
    /// <summary>
    /// Multiple identifiers may exist, either due to duplicate registration, regional rules, needs of different communication technologies, etc.
    /// </summary>
    public List<NamingSystemUniqueId> UniqueId { get; set; }
    /// <summary>
    /// Can be a urn:uuid: or a urn:oid: but real http: addresses are preferred.  Multiple instances may share the same URL if they have a distinct version.
    /// The determination of when to create a new version of a resource (same url, new version) vs. defining a new artifact is up to the author.  Considerations for making this decision are found in [Technical and Business Versions](resource.html#versions). 
    /// In some cases, the resource can no longer be found at the stated url, but the url itself cannot change. Implementations can use the [meta.source](resource.html#meta) element to indicate where the current master source of the resource can be found.
    /// </summary>
    public string Url { get; set; }
    /// <summary>
    /// Extension container element for Url
    /// </summary>
    public Element _Url { get; set; }
    /// <summary>
    /// Provides guidance on the use of the namespace, including the handling of formatting characters, use of upper vs. lower case, etc.
    /// </summary>
    public string Usage { get; set; }
    /// <summary>
    /// Extension container element for Usage
    /// </summary>
    public Element _Usage { get; set; }
    /// <summary>
    /// When multiple useContexts are specified, there is no expectation that all or any of the contexts apply.
    /// </summary>
    public List<UsageContext> UseContext { get; set; }
    /// <summary>
    /// There may be different naming system instances that have the same identifier but different versions.  The version can be appended to the url in a reference to allow a reference to a particular business version of the naming system with the format [url]|[version].
    /// </summary>
    public string Version { get; set; }
    /// <summary>
    /// Extension container element for Version
    /// </summary>
    public Element _Version { get; set; }
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


      ((fhirCsR5.Models.DomainResource)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Url))
      {
        writer.WriteString("url", (string)Url!);
      }

      if (_Url != null)
      {
        writer.WritePropertyName("_url");
        _Url.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Version))
      {
        writer.WriteString("version", (string)Version!);
      }

      if (_Version != null)
      {
        writer.WritePropertyName("_version");
        _Version.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Name))
      {
        writer.WriteString("name", (string)Name!);
      }

      if (_Name != null)
      {
        writer.WritePropertyName("_name");
        _Name.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Title))
      {
        writer.WriteString("title", (string)Title!);
      }

      if (_Title != null)
      {
        writer.WritePropertyName("_title");
        _Title.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Status))
      {
        writer.WriteString("status", (string)Status!);
      }

      if (_Status != null)
      {
        writer.WritePropertyName("_status");
        _Status.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Kind))
      {
        writer.WriteString("kind", (string)Kind!);
      }

      if (_Kind != null)
      {
        writer.WritePropertyName("_kind");
        _Kind.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Date))
      {
        writer.WriteString("date", (string)Date!);
      }

      if (_Date != null)
      {
        writer.WritePropertyName("_date");
        _Date.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Publisher))
      {
        writer.WriteString("publisher", (string)Publisher!);
      }

      if (_Publisher != null)
      {
        writer.WritePropertyName("_publisher");
        _Publisher.SerializeJson(writer, options);
      }

      if ((Contact != null) && (Contact.Count != 0))
      {
        writer.WritePropertyName("contact");
        writer.WriteStartArray();

        foreach (ContactDetail valContact in Contact)
        {
          valContact.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Responsible))
      {
        writer.WriteString("responsible", (string)Responsible!);
      }

      if (_Responsible != null)
      {
        writer.WritePropertyName("_responsible");
        _Responsible.SerializeJson(writer, options);
      }

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Description))
      {
        writer.WriteString("description", (string)Description!);
      }

      if (_Description != null)
      {
        writer.WritePropertyName("_description");
        _Description.SerializeJson(writer, options);
      }

      if ((UseContext != null) && (UseContext.Count != 0))
      {
        writer.WritePropertyName("useContext");
        writer.WriteStartArray();

        foreach (UsageContext valUseContext in UseContext)
        {
          valUseContext.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Jurisdiction != null) && (Jurisdiction.Count != 0))
      {
        writer.WritePropertyName("jurisdiction");
        writer.WriteStartArray();

        foreach (CodeableConcept valJurisdiction in Jurisdiction)
        {
          valJurisdiction.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Usage))
      {
        writer.WriteString("usage", (string)Usage!);
      }

      if (_Usage != null)
      {
        writer.WritePropertyName("_usage");
        _Usage.SerializeJson(writer, options);
      }

      if ((UniqueId != null) && (UniqueId.Count != 0))
      {
        writer.WritePropertyName("uniqueId");
        writer.WriteStartArray();

        foreach (NamingSystemUniqueId valUniqueId in UniqueId)
        {
          valUniqueId.SerializeJson(writer, options, true);
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
        case "contact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Contact = new List<ContactDetail>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.ContactDetail objContact = new fhirCsR5.Models.ContactDetail();
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

        case "date":
          Date = reader.GetString();
          break;

        case "_date":
          _Date = new fhirCsR5.Models.Element();
          _Date.DeserializeJson(ref reader, options);
          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR5.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "jurisdiction":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Jurisdiction = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.CodeableConcept objJurisdiction = new fhirCsR5.Models.CodeableConcept();
            objJurisdiction.DeserializeJson(ref reader, options);
            Jurisdiction.Add(objJurisdiction);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Jurisdiction.Count == 0)
          {
            Jurisdiction = null;
          }

          break;

        case "kind":
          Kind = reader.GetString();
          break;

        case "_kind":
          _Kind = new fhirCsR5.Models.Element();
          _Kind.DeserializeJson(ref reader, options);
          break;

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR5.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "publisher":
          Publisher = reader.GetString();
          break;

        case "_publisher":
          _Publisher = new fhirCsR5.Models.Element();
          _Publisher.DeserializeJson(ref reader, options);
          break;

        case "responsible":
          Responsible = reader.GetString();
          break;

        case "_responsible":
          _Responsible = new fhirCsR5.Models.Element();
          _Responsible.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR5.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "title":
          Title = reader.GetString();
          break;

        case "_title":
          _Title = new fhirCsR5.Models.Element();
          _Title.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = new fhirCsR5.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
          break;

        case "uniqueId":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          UniqueId = new List<NamingSystemUniqueId>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.NamingSystemUniqueId objUniqueId = new fhirCsR5.Models.NamingSystemUniqueId();
            objUniqueId.DeserializeJson(ref reader, options);
            UniqueId.Add(objUniqueId);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (UniqueId.Count == 0)
          {
            UniqueId = null;
          }

          break;

        case "url":
          Url = reader.GetString();
          break;

        case "_url":
          _Url = new fhirCsR5.Models.Element();
          _Url.DeserializeJson(ref reader, options);
          break;

        case "usage":
          Usage = reader.GetString();
          break;

        case "_usage":
          _Usage = new fhirCsR5.Models.Element();
          _Usage.DeserializeJson(ref reader, options);
          break;

        case "useContext":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          UseContext = new List<UsageContext>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.UsageContext objUseContext = new fhirCsR5.Models.UsageContext();
            objUseContext.DeserializeJson(ref reader, options);
            UseContext.Add(objUseContext);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (UseContext.Count == 0)
          {
            UseContext = null;
          }

          break;

        case "version":
          Version = reader.GetString();
          break;

        case "_version":
          _Version = new fhirCsR5.Models.Element();
          _Version.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the NamingSystem.kind field
  /// </summary>
  public static class NamingSystemKindCodes {
    public const string CODESYSTEM = "codesystem";
    public const string IDENTIFIER = "identifier";
    public const string ROOT = "root";
  }
  /// <summary>
  /// Code Values for the NamingSystem.status field
  /// </summary>
  public static class NamingSystemStatusCodes {
    public const string DRAFT = "draft";
    public const string ACTIVE = "active";
    public const string RETIRED = "retired";
    public const string UNKNOWN = "unknown";
  }
}
