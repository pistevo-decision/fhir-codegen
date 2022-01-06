// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR3.Serialization;

namespace fhirCsR3.Models
{
  /// <summary>
  /// Identifies the resource (or resources) that are being addressed by the event.  For example, the Encounter for an admit message or two Account records for a merge.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<MessageDefinitionFocus>))]
  public class MessageDefinitionFocus : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Multiple focuses addressing different resources may occasionally occur.  E.g. to link or unlink a resource from a particular account or encounter, etc.
    /// </summary>
    public string Code { get; set; }
    /// <summary>
    /// Extension container element for Code
    /// </summary>
    public Element _Code { get; set; }
    /// <summary>
    /// Identifies the maximum number of resources of this type that must be pointed to by a message in order for it to be valid against this MessageDefinition.
    /// </summary>
    public string Max { get; set; }
    /// <summary>
    /// Extension container element for Max
    /// </summary>
    public Element _Max { get; set; }
    /// <summary>
    /// Identifies the minimum number of resources of this type that must be pointed to by a message in order for it to be valid against this MessageDefinition.
    /// </summary>
    public uint? Min { get; set; }
    /// <summary>
    /// This should be present for most message definitions.  However, if the message focus is only a single resource and there is no need to include referenced resources or otherwise enforce the presence of particular elements, it can be omitted.
    /// </summary>
    public Reference Profile { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR3.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Code))
      {
        writer.WriteString("code", (string)Code!);
      }

      if (_Code != null)
      {
        writer.WritePropertyName("_code");
        _Code.SerializeJson(writer, options);
      }

      if (Profile != null)
      {
        writer.WritePropertyName("profile");
        Profile.SerializeJson(writer, options);
      }

      if (Min != null)
      {
        writer.WriteNumber("min", (uint)Min!);
      }

      if (!string.IsNullOrEmpty(Max))
      {
        writer.WriteString("max", (string)Max!);
      }

      if (_Max != null)
      {
        writer.WritePropertyName("_max");
        _Max.SerializeJson(writer, options);
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
          Code = reader.GetString();
          break;

        case "_code":
          _Code = new fhirCsR3.Models.Element();
          _Code.DeserializeJson(ref reader, options);
          break;

        case "max":
          Max = reader.GetString();
          break;

        case "_max":
          _Max = new fhirCsR3.Models.Element();
          _Max.DeserializeJson(ref reader, options);
          break;

        case "min":
          Min = reader.GetUInt32();
          break;

        case "profile":
          Profile = new fhirCsR3.Models.Reference();
          Profile.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// This indicates an application level response to "close" a transaction implicit in a particular request message.  To define a complete workflow scenario, look to the [[PlanDefinition]] resource which allows the definition of complex orchestrations, conditionality, etc.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<MessageDefinitionAllowedResponse>))]
  public class MessageDefinitionAllowedResponse : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// A reference to the message definition that must be adhered to by this supported response.
    /// </summary>
    public Reference Message { get; set; }
    /// <summary>
    /// Provides a description of the circumstances in which this response should be used (as opposed to one of the alternative responses).
    /// </summary>
    public string Situation { get; set; }
    /// <summary>
    /// Extension container element for Situation
    /// </summary>
    public Element _Situation { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR3.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Message != null)
      {
        writer.WritePropertyName("message");
        Message.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Situation))
      {
        writer.WriteString("situation", (string)Situation!);
      }

      if (_Situation != null)
      {
        writer.WritePropertyName("_situation");
        _Situation.SerializeJson(writer, options);
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
        case "message":
          Message = new fhirCsR3.Models.Reference();
          Message.DeserializeJson(ref reader, options);
          break;

        case "situation":
          Situation = reader.GetString();
          break;

        case "_situation":
          _Situation = new fhirCsR3.Models.Element();
          _Situation.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Defines the characteristics of a message that can be shared between systems, including the type of event that initiates the message, the content to be transmitted and what response(s), if any, are permitted.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<MessageDefinition>))]
  public class MessageDefinition : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "MessageDefinition";
    /// <summary>
    /// This indicates an application level response to "close" a transaction implicit in a particular request message.  To define a complete workflow scenario, look to the [[PlanDefinition]] resource which allows the definition of complex orchestrations, conditionality, etc.
    /// </summary>
    public List<MessageDefinitionAllowedResponse> AllowedResponse { get; set; }
    /// <summary>
    /// The MessageDefinition that is the basis for the contents of this resource.
    /// </summary>
    public Reference Base { get; set; }
    /// <summary>
    /// The impact of the content of the message.
    /// </summary>
    public string Category { get; set; }
    /// <summary>
    /// Extension container element for Category
    /// </summary>
    public Element _Category { get; set; }
    /// <summary>
    /// May be a web site, an email address, a telephone number, etc.
    /// </summary>
    public List<ContactDetail> Contact { get; set; }
    /// <summary>
    /// A copyright statement relating to the message definition and/or its contents. Copyright statements are generally legal restrictions on the use and publishing of the message definition.
    /// </summary>
    public string Copyright { get; set; }
    /// <summary>
    /// Extension container element for Copyright
    /// </summary>
    public Element _Copyright { get; set; }
    /// <summary>
    /// Note that this is not the same as the resource last-modified-date, since the resource may be a secondary representation of the message definition. Additional specific dates may be added as extensions or be found by consulting Provenances associated with past versions of the resource.
    /// </summary>
    public string Date { get; set; }
    /// <summary>
    /// Extension container element for Date
    /// </summary>
    public Element _Date { get; set; }
    /// <summary>
    /// This description can be used to capture details such as why the message definition was built, comments about misuse, instructions for clinical use and interpretation, literature references, examples from the paper world, etc. It is not a rendering of the message definition as conveyed in the 'text' field of the resource itself. This item SHOULD be populated unless the information is available from context (e.g. the language of the profile is presumed to be the predominant language in the place the profile was created).
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// A coded identifier of a supported messaging event.
    /// </summary>
    public Coding Event { get; set; }
    /// <summary>
    /// Allows filtering of message definition that are appropriate for use vs. not. This is labeled as "Is Modifier" because applications should not use an experimental message definition in production.
    /// </summary>
    public bool? Experimental { get; set; }
    /// <summary>
    /// Identifies the resource (or resources) that are being addressed by the event.  For example, the Encounter for an admit message or two Account records for a merge.
    /// </summary>
    public List<MessageDefinitionFocus> Focus { get; set; }
    /// <summary>
    /// Typically, this is used for identifiers that can go in an HL7 V3 II (instance identifier) data type, e.g., to identify this message definition outside of FHIR, where it is not possible to use the logical URI.
    /// </summary>
    public Identifier Identifier { get; set; }
    /// <summary>
    /// It may be possible for the message definition to be used in jurisdictions other than those for which it was originally designed or intended.
    /// </summary>
    public List<CodeableConcept> Jurisdiction { get; set; }
    /// <summary>
    /// The name is not expected to be globally unique. The name should be a simple alpha-numeric type name to ensure that it is computable friendly.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// Identifies a protocol or workflow that this MessageDefinition represents a step in.
    /// </summary>
    public List<Reference> Parent { get; set; }
    /// <summary>
    /// Usually an organization, but may be an individual. The publisher (or steward) of the message definition is the organization or individual primarily responsible for the maintenance and upkeep of the message definition. This is not necessarily the same individual or organization that developed and initially authored the content. The publisher is the primary point of contact for questions or issues with the message definition. This item SHOULD be populated unless the information is available from context.
    /// </summary>
    public string Publisher { get; set; }
    /// <summary>
    /// Extension container element for Publisher
    /// </summary>
    public Element _Publisher { get; set; }
    /// <summary>
    /// This element does not describe the usage of the message definition Instead  it provides traceability of ''why'' the resource is either needed or ''why'' it is defined as it is.  This may be used to point to source materials or specifications that drove the structure of this message definition.
    /// </summary>
    public string Purpose { get; set; }
    /// <summary>
    /// Extension container element for Purpose
    /// </summary>
    public Element _Purpose { get; set; }
    /// <summary>
    /// A MessageDefinition that is superseded by this definition.
    /// </summary>
    public List<Reference> Replaces { get; set; }
    /// <summary>
    /// Indicates whether a response is required for this message.
    /// </summary>
    public bool? ResponseRequired { get; set; }
    /// <summary>
    /// Allows filtering of message definitions that are appropriate for use vs. not.  
    /// This is labeled as "Is Modifier" because applications should not use a retired message definition without due consideration.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// This name does not need to be machine-processing friendly and may contain punctuation, white-space, etc.
    /// </summary>
    public string Title { get; set; }
    /// <summary>
    /// Extension container element for Title
    /// </summary>
    public Element _Title { get; set; }
    /// <summary>
    /// Can be a urn:uuid: or a urn:oid:, but real http: addresses are preferred.  Multiple instances may share the same url if they have a distinct version.
    /// </summary>
    public string Url { get; set; }
    /// <summary>
    /// Extension container element for Url
    /// </summary>
    public Element _Url { get; set; }
    /// <summary>
    /// When multiple useContexts are specified, there is no expectation whether all or any of the contexts apply.
    /// </summary>
    public List<UsageContext> UseContext { get; set; }
    /// <summary>
    /// There may be different message definition instances that have the same identifier but different versions.  The version can be appended to the url in a reference to allow a refrence to a particular business version of the message definition with the format [url]|[version].
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


      ((fhirCsR3.Models.DomainResource)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Url))
      {
        writer.WriteString("url", (string)Url!);
      }

      if (_Url != null)
      {
        writer.WritePropertyName("_url");
        _Url.SerializeJson(writer, options);
      }

      if (Identifier != null)
      {
        writer.WritePropertyName("identifier");
        Identifier.SerializeJson(writer, options);
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

      if (Experimental != null)
      {
        writer.WriteBoolean("experimental", (bool)Experimental!);
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

      if (!string.IsNullOrEmpty(Purpose))
      {
        writer.WriteString("purpose", (string)Purpose!);
      }

      if (_Purpose != null)
      {
        writer.WritePropertyName("_purpose");
        _Purpose.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Copyright))
      {
        writer.WriteString("copyright", (string)Copyright!);
      }

      if (_Copyright != null)
      {
        writer.WritePropertyName("_copyright");
        _Copyright.SerializeJson(writer, options);
      }

      if (Base != null)
      {
        writer.WritePropertyName("base");
        Base.SerializeJson(writer, options);
      }

      if ((Parent != null) && (Parent.Count != 0))
      {
        writer.WritePropertyName("parent");
        writer.WriteStartArray();

        foreach (Reference valParent in Parent)
        {
          valParent.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Replaces != null) && (Replaces.Count != 0))
      {
        writer.WritePropertyName("replaces");
        writer.WriteStartArray();

        foreach (Reference valReplaces in Replaces)
        {
          valReplaces.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Event != null)
      {
        writer.WritePropertyName("event");
        Event.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Category))
      {
        writer.WriteString("category", (string)Category!);
      }

      if (_Category != null)
      {
        writer.WritePropertyName("_category");
        _Category.SerializeJson(writer, options);
      }

      if ((Focus != null) && (Focus.Count != 0))
      {
        writer.WritePropertyName("focus");
        writer.WriteStartArray();

        foreach (MessageDefinitionFocus valFocus in Focus)
        {
          valFocus.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (ResponseRequired != null)
      {
        writer.WriteBoolean("responseRequired", (bool)ResponseRequired!);
      }

      if ((AllowedResponse != null) && (AllowedResponse.Count != 0))
      {
        writer.WritePropertyName("allowedResponse");
        writer.WriteStartArray();

        foreach (MessageDefinitionAllowedResponse valAllowedResponse in AllowedResponse)
        {
          valAllowedResponse.SerializeJson(writer, options, true);
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
        case "allowedResponse":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          AllowedResponse = new List<MessageDefinitionAllowedResponse>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.MessageDefinitionAllowedResponse objAllowedResponse = new fhirCsR3.Models.MessageDefinitionAllowedResponse();
            objAllowedResponse.DeserializeJson(ref reader, options);
            AllowedResponse.Add(objAllowedResponse);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (AllowedResponse.Count == 0)
          {
            AllowedResponse = null;
          }

          break;

        case "base":
          Base = new fhirCsR3.Models.Reference();
          Base.DeserializeJson(ref reader, options);
          break;

        case "category":
          Category = reader.GetString();
          break;

        case "_category":
          _Category = new fhirCsR3.Models.Element();
          _Category.DeserializeJson(ref reader, options);
          break;

        case "contact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Contact = new List<ContactDetail>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.ContactDetail objContact = new fhirCsR3.Models.ContactDetail();
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

        case "copyright":
          Copyright = reader.GetString();
          break;

        case "_copyright":
          _Copyright = new fhirCsR3.Models.Element();
          _Copyright.DeserializeJson(ref reader, options);
          break;

        case "date":
          Date = reader.GetString();
          break;

        case "_date":
          _Date = new fhirCsR3.Models.Element();
          _Date.DeserializeJson(ref reader, options);
          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR3.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "event":
          Event = new fhirCsR3.Models.Coding();
          Event.DeserializeJson(ref reader, options);
          break;

        case "experimental":
          Experimental = reader.GetBoolean();
          break;

        case "focus":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Focus = new List<MessageDefinitionFocus>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.MessageDefinitionFocus objFocus = new fhirCsR3.Models.MessageDefinitionFocus();
            objFocus.DeserializeJson(ref reader, options);
            Focus.Add(objFocus);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Focus.Count == 0)
          {
            Focus = null;
          }

          break;

        case "identifier":
          Identifier = new fhirCsR3.Models.Identifier();
          Identifier.DeserializeJson(ref reader, options);
          break;

        case "jurisdiction":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Jurisdiction = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objJurisdiction = new fhirCsR3.Models.CodeableConcept();
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

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR3.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "parent":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Parent = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objParent = new fhirCsR3.Models.Reference();
            objParent.DeserializeJson(ref reader, options);
            Parent.Add(objParent);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Parent.Count == 0)
          {
            Parent = null;
          }

          break;

        case "publisher":
          Publisher = reader.GetString();
          break;

        case "_publisher":
          _Publisher = new fhirCsR3.Models.Element();
          _Publisher.DeserializeJson(ref reader, options);
          break;

        case "purpose":
          Purpose = reader.GetString();
          break;

        case "_purpose":
          _Purpose = new fhirCsR3.Models.Element();
          _Purpose.DeserializeJson(ref reader, options);
          break;

        case "replaces":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Replaces = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objReplaces = new fhirCsR3.Models.Reference();
            objReplaces.DeserializeJson(ref reader, options);
            Replaces.Add(objReplaces);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Replaces.Count == 0)
          {
            Replaces = null;
          }

          break;

        case "responseRequired":
          ResponseRequired = reader.GetBoolean();
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR3.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "title":
          Title = reader.GetString();
          break;

        case "_title":
          _Title = new fhirCsR3.Models.Element();
          _Title.DeserializeJson(ref reader, options);
          break;

        case "url":
          Url = reader.GetString();
          break;

        case "_url":
          _Url = new fhirCsR3.Models.Element();
          _Url.DeserializeJson(ref reader, options);
          break;

        case "useContext":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          UseContext = new List<UsageContext>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.UsageContext objUseContext = new fhirCsR3.Models.UsageContext();
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
          _Version = new fhirCsR3.Models.Element();
          _Version.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the MessageDefinition.category field
  /// </summary>
  public static class MessageDefinitionCategoryCodes {
    public const string CONSEQUENCE = "Consequence";
    public const string CURRENCY = "Currency";
    public const string NOTIFICATION = "Notification";
  }
  /// <summary>
  /// Code Values for the MessageDefinition.status field
  /// </summary>
  public static class MessageDefinitionStatusCodes {
    public const string DRAFT = "draft";
    public const string ACTIVE = "active";
    public const string RETIRED = "retired";
    public const string UNKNOWN = "unknown";
  }
}
