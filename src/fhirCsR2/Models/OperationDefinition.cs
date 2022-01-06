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
  /// Contacts to assist a user in finding and communicating with the publisher.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<OperationDefinitionContact>))]
  public class OperationDefinitionContact : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The name of an individual to contact regarding the operation definition.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// Contact details for individual (if a name was provided) or the publisher.
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

      if (!string.IsNullOrEmpty(Name))
      {
        writer.WriteString("name", (string)Name!);
      }

      if (_Name != null)
      {
        writer.WritePropertyName("_name");
        _Name.SerializeJson(writer, options);
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
        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR2.Models.Element();
          _Name.DeserializeJson(ref reader, options);
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
  /// Binds to a value set if this parameter is coded (code, Coding, CodeableConcept).
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<OperationDefinitionParameterBinding>))]
  public class OperationDefinitionParameterBinding : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Indicates the degree of conformance expectations associated with this binding - that is, the degree to which the provided value set must be adhered to in the instances.
    /// </summary>
    public string Strength { get; set; }
    /// <summary>
    /// Extension container element for Strength
    /// </summary>
    public Element _Strength { get; set; }
    /// <summary>
    /// Points to the value set or external definition (e.g. implicit value set) that identifies the set of codes to be used.
    /// </summary>
    public string ValueSetUri { get; set; }
    /// <summary>
    /// Extension container element for ValueSetUri
    /// </summary>
    public Element _ValueSetUri { get; set; }
    /// <summary>
    /// Points to the value set or external definition (e.g. implicit value set) that identifies the set of codes to be used.
    /// </summary>
    public Reference ValueSetReference { get; set; }
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

      if (!string.IsNullOrEmpty(Strength))
      {
        writer.WriteString("strength", (string)Strength!);
      }

      if (_Strength != null)
      {
        writer.WritePropertyName("_strength");
        _Strength.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ValueSetUri))
      {
        writer.WriteString("valueSetUri", (string)ValueSetUri!);
      }

      if (_ValueSetUri != null)
      {
        writer.WritePropertyName("_valueSetUri");
        _ValueSetUri.SerializeJson(writer, options);
      }

      if (ValueSetReference != null)
      {
        writer.WritePropertyName("valueSetReference");
        ValueSetReference.SerializeJson(writer, options);
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
        case "strength":
          Strength = reader.GetString();
          break;

        case "_strength":
          _Strength = new fhirCsR2.Models.Element();
          _Strength.DeserializeJson(ref reader, options);
          break;

        case "valueSetUri":
          ValueSetUri = reader.GetString();
          break;

        case "_valueSetUri":
          _ValueSetUri = new fhirCsR2.Models.Element();
          _ValueSetUri.DeserializeJson(ref reader, options);
          break;

        case "valueSetReference":
          ValueSetReference = new fhirCsR2.Models.Reference();
          ValueSetReference.DeserializeJson(ref reader, options);
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
  /// Code Values for the OperationDefinition.parameter.binding.strength field
  /// </summary>
  public static class OperationDefinitionParameterBindingStrengthCodes {
    public const string REQUIRED = "required";
    public const string EXTENSIBLE = "extensible";
    public const string PREFERRED = "preferred";
    public const string EXAMPLE = "example";
  }
  /// <summary>
  /// The parameters for the operation/query.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<OperationDefinitionParameter>))]
  public class OperationDefinitionParameter : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Binds to a value set if this parameter is coded (code, Coding, CodeableConcept).
    /// </summary>
    public OperationDefinitionParameterBinding Binding { get; set; }
    /// <summary>
    /// Describes the meaning or use of this parameter.
    /// </summary>
    public string Documentation { get; set; }
    /// <summary>
    /// Extension container element for Documentation
    /// </summary>
    public Element _Documentation { get; set; }
    /// <summary>
    /// The maximum number of times this element is permitted to appear in the request or response.
    /// </summary>
    public string Max { get; set; }
    /// <summary>
    /// Extension container element for Max
    /// </summary>
    public Element _Max { get; set; }
    /// <summary>
    /// The minimum number of times this parameter SHALL appear in the request or response.
    /// </summary>
    public int Min { get; set; }
    /// <summary>
    /// Extension container element for Min
    /// </summary>
    public Element _Min { get; set; }
    /// <summary>
    /// The name of used to identify the parameter.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// The parts of a Tuple Parameter.
    /// </summary>
    public List<OperationDefinitionParameter> Part { get; set; }
    /// <summary>
    /// A profile the specifies the rules that this parameter must conform to.
    /// </summary>
    public Reference Profile { get; set; }
    /// <summary>
    /// The type for this parameter.
    /// </summary>
    public string Type { get; set; }
    /// <summary>
    /// Extension container element for Type
    /// </summary>
    public Element _Type { get; set; }
    /// <summary>
    /// Whether this is an input or an output parameter.
    /// </summary>
    public string Use { get; set; }
    /// <summary>
    /// Extension container element for Use
    /// </summary>
    public Element _Use { get; set; }
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

      if (!string.IsNullOrEmpty(Name))
      {
        writer.WriteString("name", (string)Name!);
      }

      if (_Name != null)
      {
        writer.WritePropertyName("_name");
        _Name.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Use))
      {
        writer.WriteString("use", (string)Use!);
      }

      if (_Use != null)
      {
        writer.WritePropertyName("_use");
        _Use.SerializeJson(writer, options);
      }

      writer.WriteNumber("min", Min);

      if (_Min != null)
      {
        writer.WritePropertyName("_min");
        _Min.SerializeJson(writer, options);
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

      if (!string.IsNullOrEmpty(Documentation))
      {
        writer.WriteString("documentation", (string)Documentation!);
      }

      if (_Documentation != null)
      {
        writer.WritePropertyName("_documentation");
        _Documentation.SerializeJson(writer, options);
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

      if (Profile != null)
      {
        writer.WritePropertyName("profile");
        Profile.SerializeJson(writer, options);
      }

      if (Binding != null)
      {
        writer.WritePropertyName("binding");
        Binding.SerializeJson(writer, options);
      }

      if ((Part != null) && (Part.Count != 0))
      {
        writer.WritePropertyName("part");
        writer.WriteStartArray();

        foreach (OperationDefinitionParameter valPart in Part)
        {
          valPart.SerializeJson(writer, options, true);
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
        case "binding":
          Binding = new fhirCsR2.Models.OperationDefinitionParameterBinding();
          Binding.DeserializeJson(ref reader, options);
          break;

        case "documentation":
          Documentation = reader.GetString();
          break;

        case "_documentation":
          _Documentation = new fhirCsR2.Models.Element();
          _Documentation.DeserializeJson(ref reader, options);
          break;

        case "max":
          Max = reader.GetString();
          break;

        case "_max":
          _Max = new fhirCsR2.Models.Element();
          _Max.DeserializeJson(ref reader, options);
          break;

        case "min":
          Min = reader.GetInt32();
          break;

        case "_min":
          _Min = new fhirCsR2.Models.Element();
          _Min.DeserializeJson(ref reader, options);
          break;

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR2.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "part":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Part = new List<OperationDefinitionParameter>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.OperationDefinitionParameter objPart = new fhirCsR2.Models.OperationDefinitionParameter();
            objPart.DeserializeJson(ref reader, options);
            Part.Add(objPart);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Part.Count == 0)
          {
            Part = null;
          }

          break;

        case "profile":
          Profile = new fhirCsR2.Models.Reference();
          Profile.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = reader.GetString();
          break;

        case "_type":
          _Type = new fhirCsR2.Models.Element();
          _Type.DeserializeJson(ref reader, options);
          break;

        case "use":
          Use = reader.GetString();
          break;

        case "_use":
          _Use = new fhirCsR2.Models.Element();
          _Use.DeserializeJson(ref reader, options);
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
  /// Code Values for the OperationDefinition.parameter.use field
  /// </summary>
  public static class OperationDefinitionParameterUseCodes {
    public const string VAL_IN = "in";
    public const string VAL_OUT = "out";
  }
  /// <summary>
  /// A formal computable definition of an operation (on the RESTful interface) or a named query (using the search interaction).
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<OperationDefinition>))]
  public class OperationDefinition : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "OperationDefinition";
    /// <summary>
    /// Indicates that this operation definition is a constraining profile on the base.
    /// </summary>
    public Reference Base { get; set; }
    /// <summary>
    /// The name used to invoke the operation.
    /// </summary>
    public string Code { get; set; }
    /// <summary>
    /// Extension container element for Code
    /// </summary>
    public Element _Code { get; set; }
    /// <summary>
    /// Contacts to assist a user in finding and communicating with the publisher.
    /// </summary>
    public List<OperationDefinitionContact> Contact { get; set; }
    /// <summary>
    /// The date this version of the operation definition was published. The date must change when the business version changes, if it does, and it must change if the status code changes. In addition, it should change when the substantive content of the Operation Definition changes.
    /// </summary>
    public string Date { get; set; }
    /// <summary>
    /// Extension container element for Date
    /// </summary>
    public Element _Date { get; set; }
    /// <summary>
    /// A free text natural language description of the profile and its use.
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// Allows filtering of profiles that are appropriate for use vs. not.
    /// </summary>
    public bool? Experimental { get; set; }
    /// <summary>
    /// Extension container element for Experimental
    /// </summary>
    public Element _Experimental { get; set; }
    /// <summary>
    /// Operations that are idempotent (see [HTTP specification definition of idempotent](http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html)) may be invoked by performing an HTTP GET operation instead of a POST.
    /// </summary>
    public bool? Idempotent { get; set; }
    /// <summary>
    /// Extension container element for Idempotent
    /// </summary>
    public Element _Idempotent { get; set; }
    /// <summary>
    /// Indicates whether this operation can be invoked on a particular instance of one of the given types.
    /// </summary>
    public bool Instance { get; set; }
    /// <summary>
    /// Extension container element for Instance
    /// </summary>
    public Element _Instance { get; set; }
    /// <summary>
    /// Whether this is an operation or a named query.
    /// </summary>
    public string Kind { get; set; }
    /// <summary>
    /// Extension container element for Kind
    /// </summary>
    public Element _Kind { get; set; }
    /// <summary>
    /// A free text natural language name identifying the operation.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// Additional information about how to use this operation or named query.
    /// </summary>
    public string Notes { get; set; }
    /// <summary>
    /// Extension container element for Notes
    /// </summary>
    public Element _Notes { get; set; }
    /// <summary>
    /// The parameters for the operation/query.
    /// </summary>
    public List<OperationDefinitionParameter> Parameter { get; set; }
    /// <summary>
    /// Helps establish the "authority/credibility" of the operation definition.  May also allow for contact.
    /// </summary>
    public string Publisher { get; set; }
    /// <summary>
    /// Extension container element for Publisher
    /// </summary>
    public Element _Publisher { get; set; }
    /// <summary>
    /// Explains why this operation definition is needed and why it's been constrained as it has.
    /// </summary>
    public string Requirements { get; set; }
    /// <summary>
    /// Extension container element for Requirements
    /// </summary>
    public Element _Requirements { get; set; }
    /// <summary>
    /// Allows filtering of profiles that are appropriate for use vs. not.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// Indicates whether this operation or named query can be invoked at the system level (e.g. without needing to choose a resource type for the context).
    /// </summary>
    public bool System { get; set; }
    /// <summary>
    /// Extension container element for System
    /// </summary>
    public Element _System { get; set; }
    /// <summary>
    /// Indicates whether this operation or named query can be invoked at the resource type level for any given resource type level (e.g. without needing to choose a resource type for the context).
    /// </summary>
    public List<string> Type { get; set; }
    /// <summary>
    /// Extension container element for Type
    /// </summary>
    public List<Element> _Type { get; set; }
    /// <summary>
    /// An absolute URL that is used to identify this operation definition when it is referenced in a specification, model, design or an instance. This SHALL be a URL, SHOULD be globally unique, and SHOULD be an address at which this operation definition is (or will be) published.
    /// </summary>
    public string Url { get; set; }
    /// <summary>
    /// Extension container element for Url
    /// </summary>
    public Element _Url { get; set; }
    /// <summary>
    /// There may be multiple resource versions of the profile that have this same identifier. The resource version id will change for technical reasons, whereas the stated version number needs to be under the author's control.
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


      ((fhirCsR2.Models.DomainResource)this).SerializeJson(writer, options, false);

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

      if (Experimental != null)
      {
        writer.WriteBoolean("experimental", (bool)Experimental!);
      }

      if (_Experimental != null)
      {
        writer.WritePropertyName("_experimental");
        _Experimental.SerializeJson(writer, options);
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

        foreach (OperationDefinitionContact valContact in Contact)
        {
          valContact.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
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

      if (!string.IsNullOrEmpty(Description))
      {
        writer.WriteString("description", (string)Description!);
      }

      if (_Description != null)
      {
        writer.WritePropertyName("_description");
        _Description.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Requirements))
      {
        writer.WriteString("requirements", (string)Requirements!);
      }

      if (_Requirements != null)
      {
        writer.WritePropertyName("_requirements");
        _Requirements.SerializeJson(writer, options);
      }

      if (Idempotent != null)
      {
        writer.WriteBoolean("idempotent", (bool)Idempotent!);
      }

      if (_Idempotent != null)
      {
        writer.WritePropertyName("_idempotent");
        _Idempotent.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Code))
      {
        writer.WriteString("code", (string)Code!);
      }

      if (_Code != null)
      {
        writer.WritePropertyName("_code");
        _Code.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Notes))
      {
        writer.WriteString("notes", (string)Notes!);
      }

      if (_Notes != null)
      {
        writer.WritePropertyName("_notes");
        _Notes.SerializeJson(writer, options);
      }

      if (Base != null)
      {
        writer.WritePropertyName("base");
        Base.SerializeJson(writer, options);
      }

      writer.WriteBoolean("system", System);

      if (_System != null)
      {
        writer.WritePropertyName("_system");
        _System.SerializeJson(writer, options);
      }

      if ((Type != null) && (Type.Count != 0))
      {
        writer.WritePropertyName("type");
        writer.WriteStartArray();

        foreach (string valType in Type)
        {
          writer.WriteStringValue(valType);
        }

        writer.WriteEndArray();
      }

      if ((_Type != null) && (_Type.Count != 0))
      {
        writer.WritePropertyName("_type");
        writer.WriteStartArray();

        foreach (Element val_Type in _Type)
        {
          val_Type.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      writer.WriteBoolean("instance", Instance);

      if (_Instance != null)
      {
        writer.WritePropertyName("_instance");
        _Instance.SerializeJson(writer, options);
      }

      if ((Parameter != null) && (Parameter.Count != 0))
      {
        writer.WritePropertyName("parameter");
        writer.WriteStartArray();

        foreach (OperationDefinitionParameter valParameter in Parameter)
        {
          valParameter.SerializeJson(writer, options, true);
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
        case "base":
          Base = new fhirCsR2.Models.Reference();
          Base.DeserializeJson(ref reader, options);
          break;

        case "code":
          Code = reader.GetString();
          break;

        case "_code":
          _Code = new fhirCsR2.Models.Element();
          _Code.DeserializeJson(ref reader, options);
          break;

        case "contact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Contact = new List<OperationDefinitionContact>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.OperationDefinitionContact objContact = new fhirCsR2.Models.OperationDefinitionContact();
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
          _Date = new fhirCsR2.Models.Element();
          _Date.DeserializeJson(ref reader, options);
          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR2.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "experimental":
          Experimental = reader.GetBoolean();
          break;

        case "_experimental":
          _Experimental = new fhirCsR2.Models.Element();
          _Experimental.DeserializeJson(ref reader, options);
          break;

        case "idempotent":
          Idempotent = reader.GetBoolean();
          break;

        case "_idempotent":
          _Idempotent = new fhirCsR2.Models.Element();
          _Idempotent.DeserializeJson(ref reader, options);
          break;

        case "instance":
          Instance = reader.GetBoolean();
          break;

        case "_instance":
          _Instance = new fhirCsR2.Models.Element();
          _Instance.DeserializeJson(ref reader, options);
          break;

        case "kind":
          Kind = reader.GetString();
          break;

        case "_kind":
          _Kind = new fhirCsR2.Models.Element();
          _Kind.DeserializeJson(ref reader, options);
          break;

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR2.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "notes":
          Notes = reader.GetString();
          break;

        case "_notes":
          _Notes = new fhirCsR2.Models.Element();
          _Notes.DeserializeJson(ref reader, options);
          break;

        case "parameter":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Parameter = new List<OperationDefinitionParameter>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.OperationDefinitionParameter objParameter = new fhirCsR2.Models.OperationDefinitionParameter();
            objParameter.DeserializeJson(ref reader, options);
            Parameter.Add(objParameter);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Parameter.Count == 0)
          {
            Parameter = null;
          }

          break;

        case "publisher":
          Publisher = reader.GetString();
          break;

        case "_publisher":
          _Publisher = new fhirCsR2.Models.Element();
          _Publisher.DeserializeJson(ref reader, options);
          break;

        case "requirements":
          Requirements = reader.GetString();
          break;

        case "_requirements":
          _Requirements = new fhirCsR2.Models.Element();
          _Requirements.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR2.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "system":
          System = reader.GetBoolean();
          break;

        case "_system":
          _System = new fhirCsR2.Models.Element();
          _System.DeserializeJson(ref reader, options);
          break;

        case "type":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Type = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Type.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Type.Count == 0)
          {
            Type = null;
          }

          break;

        case "_type":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _Type = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.Element obj_Type = new fhirCsR2.Models.Element();
            obj_Type.DeserializeJson(ref reader, options);
            _Type.Add(obj_Type);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_Type.Count == 0)
          {
            _Type = null;
          }

          break;

        case "url":
          Url = reader.GetString();
          break;

        case "_url":
          _Url = new fhirCsR2.Models.Element();
          _Url.DeserializeJson(ref reader, options);
          break;

        case "version":
          Version = reader.GetString();
          break;

        case "_version":
          _Version = new fhirCsR2.Models.Element();
          _Version.DeserializeJson(ref reader, options);
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
  /// Code Values for the OperationDefinition.kind field
  /// </summary>
  public static class OperationDefinitionKindCodes {
    public const string OPERATION = "operation";
    public const string QUERY = "query";
  }
  /// <summary>
  /// Code Values for the OperationDefinition.status field
  /// </summary>
  public static class OperationDefinitionStatusCodes {
    public const string DRAFT = "draft";
    public const string ACTIVE = "active";
    public const string RETIRED = "retired";
  }
}
