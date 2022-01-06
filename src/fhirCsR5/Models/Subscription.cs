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
  /// The filter properties to be applied to narrow the subscription topic stream.  When multiple filters are applied, evaluates to true if all the conditions are met; otherwise it returns false.   (i.e., logical AND).
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<SubscriptionFilterBy>))]
  public class SubscriptionFilterBy : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// If the element is a reference to another resource, this element contains "Reference", and the targetProfile element defines what resources can be referenced. The targetProfile may be a reference to the general definition of a resource (e.g. http://hl7.org/fhir/StructureDefinition/Patient).
    /// </summary>
    public string ResourceType { get; set; }
    /// <summary>
    /// Extension container element for ResourceType
    /// </summary>
    public Element _ResourceType { get; set; }
    /// <summary>
    /// The operator to apply to the filter value when determining matches (Search modifiers).
    /// </summary>
    public string SearchModifier { get; set; }
    /// <summary>
    /// Extension container element for SearchModifier
    /// </summary>
    public Element _SearchModifier { get; set; }
    /// <summary>
    /// The filter label (=key) as defined in the `SubscriptionTopic.canfilterBy.searchParamName`  element.
    /// </summary>
    public string SearchParamName { get; set; }
    /// <summary>
    /// Extension container element for SearchParamName
    /// </summary>
    public Element _SearchParamName { get; set; }
    /// <summary>
    /// The literal value or resource path as is legal in search - for example, "Patient/123" or "le1950".
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

      if (!string.IsNullOrEmpty(ResourceType))
      {
        writer.WriteString("resourceType", (string)ResourceType!);
      }

      if (_ResourceType != null)
      {
        writer.WritePropertyName("_resourceType");
        _ResourceType.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(SearchParamName))
      {
        writer.WriteString("searchParamName", (string)SearchParamName!);
      }

      if (_SearchParamName != null)
      {
        writer.WritePropertyName("_searchParamName");
        _SearchParamName.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(SearchModifier))
      {
        writer.WriteString("searchModifier", (string)SearchModifier!);
      }

      if (_SearchModifier != null)
      {
        writer.WritePropertyName("_searchModifier");
        _SearchModifier.SerializeJson(writer, options);
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
        case "resourceType":
          ResourceType = reader.GetString();
          break;

        case "_resourceType":
          _ResourceType = new fhirCsR5.Models.Element();
          _ResourceType.DeserializeJson(ref reader, options);
          break;

        case "searchModifier":
          SearchModifier = reader.GetString();
          break;

        case "_searchModifier":
          _SearchModifier = new fhirCsR5.Models.Element();
          _SearchModifier.DeserializeJson(ref reader, options);
          break;

        case "searchParamName":
          SearchParamName = reader.GetString();
          break;

        case "_searchParamName":
          _SearchParamName = new fhirCsR5.Models.Element();
          _SearchParamName.DeserializeJson(ref reader, options);
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
  /// Code Values for the Subscription.filterBy.searchModifier field
  /// </summary>
  public static class SubscriptionFilterBySearchModifierCodes {
    public const string EQUALS = "=";
    public const string EQ = "eq";
    public const string NE = "ne";
    public const string GT = "gt";
    public const string LT = "lt";
    public const string GE = "ge";
    public const string LE = "le";
    public const string SA = "sa";
    public const string EB = "eb";
    public const string AP = "ap";
    public const string ABOVE = "above";
    public const string BELOW = "below";
    public const string VAL_IN = "in";
    public const string NOT_IN = "not-in";
    public const string OF_TYPE = "of-type";
  }
  /// <summary>
  /// The subscription resource describes a particular client's request to be notified about a SubscriptionTopic.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<Subscription>))]
  public class Subscription : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "Subscription";
    /// <summary>
    /// The type of channel to send notifications on.
    /// </summary>
    public Coding ChannelType { get; set; }
    /// <summary>
    /// Contact details for a human to contact about the subscription. The primary use of this for system administrator troubleshooting.
    /// </summary>
    public List<ContactPoint> Contact { get; set; }
    /// <summary>
    /// Sending the payload has obvious security implications. The server is responsible for ensuring that the content is appropriately secured.
    /// </summary>
    public string Content { get; set; }
    /// <summary>
    /// Extension container element for Content
    /// </summary>
    public Element _Content { get; set; }
    /// <summary>
    /// The mime type to send the payload in - either application/fhir+xml, or application/fhir+json. The MIME types "text/plain" and "text/html" may also be used for Email subscriptions.
    /// </summary>
    public string ContentType { get; set; }
    /// <summary>
    /// Extension container element for ContentType
    /// </summary>
    public Element _ContentType { get; set; }
    /// <summary>
    /// The server is permitted to deviate from this time but should observe it.
    /// </summary>
    public string End { get; set; }
    /// <summary>
    /// Extension container element for End
    /// </summary>
    public Element _End { get; set; }
    /// <summary>
    /// For rest-hook the end-point must be an http: or https: URL; for websocket ws: or wss:; for email, a mailto: url; and for message the endpoint can be in any form of url the server understands (usually, http/s: or mllp:). The URI is allowed to be relative; in which case, it is relative to the server end-point (since there may be more than one, clients should avoid using relative URIs).
    /// </summary>
    public string Endpoint { get; set; }
    /// <summary>
    /// Extension container element for Endpoint
    /// </summary>
    public Element _Endpoint { get; set; }
    /// <summary>
    /// The filter properties to be applied to narrow the subscription topic stream.  When multiple filters are applied, evaluates to true if all the conditions are met; otherwise it returns false.   (i.e., logical AND).
    /// </summary>
    public List<SubscriptionFilterBy> FilterBy { get; set; }
    /// <summary>
    /// Exactly what these mean depend on the channel type. They can convey additional information to the recipient and/or meet security requirements; for example, support of multiple headers in the outgoing notifications for rest-hook type subscriptions.
    /// </summary>
    public List<string> Header { get; set; }
    /// <summary>
    /// Extension container element for Header
    /// </summary>
    public List<Element> _Header { get; set; }
    /// <summary>
    /// If present,  a 'hearbeat" notification (keepalive) is sent via this channel with an the interval period equal to this elements integer value in seconds.    If not present, a heartbeat notification is not sent.
    /// </summary>
    public uint? HeartbeatPeriod { get; set; }
    /// <summary>
    /// A formal identifier that is used to identify this code system when it is represented in other formats, or referenced in a specification, model, design or an instance.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// If present, the maximum number of triggering resources that will be included in a notification bundle (e.g., a server will not include more than this number of trigger resources in a single notification).  Note that this is not a strict limit on the number of entries in a bundle, as dependent resources can be included.
    /// </summary>
    public uint? MaxCount { get; set; }
    /// <summary>
    /// A natural language name identifying the subscription.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// If present, where to place URLs of resources in notifications.
    /// </summary>
    public string NotificationUrlLocation { get; set; }
    /// <summary>
    /// Extension container element for NotificationUrlLocation
    /// </summary>
    public Element _NotificationUrlLocation { get; set; }
    /// <summary>
    /// A description of why this subscription is defined.
    /// </summary>
    public string Reason { get; set; }
    /// <summary>
    /// Extension container element for Reason
    /// </summary>
    public Element _Reason { get; set; }
    /// <summary>
    /// A client can only submit subscription resources in the requested or off state. Only the server can  move a subscription from requested to active, and then to error. Either the server or the client can turn a subscription off.
    /// This element is labeled as a modifier because the status contains codes that mark the resource as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// If present, the maximum amount of time a server will allow before failing a notification attempt.
    /// </summary>
    public uint? Timeout { get; set; }
    /// <summary>
    /// The reference to the subscription topic to be notified about.
    /// </summary>
    public string Topic { get; set; }
    /// <summary>
    /// Extension container element for Topic
    /// </summary>
    public Element _Topic { get; set; }
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

      if (!string.IsNullOrEmpty(Topic))
      {
        writer.WriteString("topic", (string)Topic!);
      }

      if (_Topic != null)
      {
        writer.WritePropertyName("_topic");
        _Topic.SerializeJson(writer, options);
      }

      if ((Contact != null) && (Contact.Count != 0))
      {
        writer.WritePropertyName("contact");
        writer.WriteStartArray();

        foreach (ContactPoint valContact in Contact)
        {
          valContact.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(End))
      {
        writer.WriteString("end", (string)End!);
      }

      if (_End != null)
      {
        writer.WritePropertyName("_end");
        _End.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Reason))
      {
        writer.WriteString("reason", (string)Reason!);
      }

      if (_Reason != null)
      {
        writer.WritePropertyName("_reason");
        _Reason.SerializeJson(writer, options);
      }

      if ((FilterBy != null) && (FilterBy.Count != 0))
      {
        writer.WritePropertyName("filterBy");
        writer.WriteStartArray();

        foreach (SubscriptionFilterBy valFilterBy in FilterBy)
        {
          valFilterBy.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (ChannelType != null)
      {
        writer.WritePropertyName("channelType");
        ChannelType.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Endpoint))
      {
        writer.WriteString("endpoint", (string)Endpoint!);
      }

      if (_Endpoint != null)
      {
        writer.WritePropertyName("_endpoint");
        _Endpoint.SerializeJson(writer, options);
      }

      if ((Header != null) && (Header.Count != 0))
      {
        writer.WritePropertyName("header");
        writer.WriteStartArray();

        foreach (string valHeader in Header)
        {
          writer.WriteStringValue(valHeader);
        }

        writer.WriteEndArray();
      }

      if ((_Header != null) && (_Header.Count != 0))
      {
        writer.WritePropertyName("_header");
        writer.WriteStartArray();

        foreach (Element val_Header in _Header)
        {
          val_Header.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (HeartbeatPeriod != null)
      {
        writer.WriteNumber("heartbeatPeriod", (uint)HeartbeatPeriod!);
      }

      if (Timeout != null)
      {
        writer.WriteNumber("timeout", (uint)Timeout!);
      }

      if (!string.IsNullOrEmpty(ContentType))
      {
        writer.WriteString("contentType", (string)ContentType!);
      }

      if (_ContentType != null)
      {
        writer.WritePropertyName("_contentType");
        _ContentType.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Content))
      {
        writer.WriteString("content", (string)Content!);
      }

      if (_Content != null)
      {
        writer.WritePropertyName("_content");
        _Content.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(NotificationUrlLocation))
      {
        writer.WriteString("notificationUrlLocation", (string)NotificationUrlLocation!);
      }

      if (_NotificationUrlLocation != null)
      {
        writer.WritePropertyName("_notificationUrlLocation");
        _NotificationUrlLocation.SerializeJson(writer, options);
      }

      if (MaxCount != null)
      {
        writer.WriteNumber("maxCount", (uint)MaxCount!);
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
        case "channelType":
          ChannelType = new fhirCsR5.Models.Coding();
          ChannelType.DeserializeJson(ref reader, options);
          break;

        case "contact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Contact = new List<ContactPoint>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.ContactPoint objContact = new fhirCsR5.Models.ContactPoint();
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

        case "content":
          Content = reader.GetString();
          break;

        case "_content":
          _Content = new fhirCsR5.Models.Element();
          _Content.DeserializeJson(ref reader, options);
          break;

        case "contentType":
          ContentType = reader.GetString();
          break;

        case "_contentType":
          _ContentType = new fhirCsR5.Models.Element();
          _ContentType.DeserializeJson(ref reader, options);
          break;

        case "end":
          End = reader.GetString();
          break;

        case "_end":
          _End = new fhirCsR5.Models.Element();
          _End.DeserializeJson(ref reader, options);
          break;

        case "endpoint":
          Endpoint = reader.GetString();
          break;

        case "_endpoint":
          _Endpoint = new fhirCsR5.Models.Element();
          _Endpoint.DeserializeJson(ref reader, options);
          break;

        case "filterBy":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          FilterBy = new List<SubscriptionFilterBy>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.SubscriptionFilterBy objFilterBy = new fhirCsR5.Models.SubscriptionFilterBy();
            objFilterBy.DeserializeJson(ref reader, options);
            FilterBy.Add(objFilterBy);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (FilterBy.Count == 0)
          {
            FilterBy = null;
          }

          break;

        case "header":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Header = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Header.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Header.Count == 0)
          {
            Header = null;
          }

          break;

        case "_header":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _Header = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Element obj_Header = new fhirCsR5.Models.Element();
            obj_Header.DeserializeJson(ref reader, options);
            _Header.Add(obj_Header);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_Header.Count == 0)
          {
            _Header = null;
          }

          break;

        case "heartbeatPeriod":
          HeartbeatPeriod = reader.GetUInt32();
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Identifier objIdentifier = new fhirCsR5.Models.Identifier();
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

        case "maxCount":
          MaxCount = reader.GetUInt32();
          break;

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR5.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "notificationUrlLocation":
          NotificationUrlLocation = reader.GetString();
          break;

        case "_notificationUrlLocation":
          _NotificationUrlLocation = new fhirCsR5.Models.Element();
          _NotificationUrlLocation.DeserializeJson(ref reader, options);
          break;

        case "reason":
          Reason = reader.GetString();
          break;

        case "_reason":
          _Reason = new fhirCsR5.Models.Element();
          _Reason.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR5.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "timeout":
          Timeout = reader.GetUInt32();
          break;

        case "topic":
          Topic = reader.GetString();
          break;

        case "_topic":
          _Topic = new fhirCsR5.Models.Element();
          _Topic.DeserializeJson(ref reader, options);
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
  /// Code Values for the Subscription.content field
  /// </summary>
  public static class SubscriptionContentCodes {
    public const string EMPTY = "empty";
    public const string ID_ONLY = "id-only";
    public const string FULL_RESOURCE = "full-resource";
  }
  /// <summary>
  /// Code Values for the Subscription.notificationUrlLocation field
  /// </summary>
  public static class SubscriptionNotificationUrlLocationCodes {
    public const string NONE = "none";
    public const string FULL_URL = "full-url";
    public const string REQUEST_RESPONSE = "request-response";
    public const string ALL = "all";
  }
  /// <summary>
  /// Code Values for the Subscription.status field
  /// </summary>
  public static class SubscriptionStatusCodes {
    public const string REQUESTED = "requested";
    public const string ACTIVE = "active";
    public const string ERROR = "error";
    public const string OFF = "off";
    public const string ENTERED_IN_ERROR = "entered-in-error";
  }
}
