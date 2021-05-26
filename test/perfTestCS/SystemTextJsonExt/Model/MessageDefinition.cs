// <auto-generated/>
// Contents of: hl7.fhir.r4.core version: 4.0.1

using System;
using System.Buffers;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using Hl7.Fhir.Model;
using Hl7.Fhir.Model.JsonExtensions;
using Hl7.Fhir.Serialization;

/*
  Copyright (c) 2011+, HL7, Inc.
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification, 
  are permitted provided that the following conditions are met:
  
   * Redistributions of source code must retain the above copyright notice, this 
     list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice, 
     this list of conditions and the following disclaimer in the documentation 
     and/or other materials provided with the distribution.
   * Neither the name of HL7 nor the names of its contributors may be used to 
     endorse or promote products derived from this software without specific 
     prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
  POSSIBILITY OF SUCH DAMAGE.
  
*/

namespace Hl7.Fhir.Model.JsonExtensions
{
  /// <summary>
  /// JSON Serialization Extensions for MessageDefinition
  /// </summary>
  public static class MessageDefinitionJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR MessageDefinition into JSON
    /// </summary>
    public static void SerializeJson(this MessageDefinition current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","MessageDefinition");
      // Complex: MessageDefinition, Export: MessageDefinition, Base: DomainResource (DomainResource)
      ((Hl7.Fhir.Model.DomainResource)current).SerializeJson(writer, options, false);

      if (current.UrlElement != null)
      {
        if (!string.IsNullOrEmpty(current.UrlElement.Value))
        {
          writer.WriteString("url",current.UrlElement.Value);
        }
        if (current.UrlElement.HasExtensions() || (!string.IsNullOrEmpty(current.UrlElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_url",false,current.UrlElement.Extension,current.UrlElement.ElementId);
        }
      }

      if ((current.Identifier != null) && (current.Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();
        foreach (Identifier val in current.Identifier)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.VersionElement != null)
      {
        if (!string.IsNullOrEmpty(current.VersionElement.Value))
        {
          writer.WriteString("version",current.VersionElement.Value);
        }
        if (current.VersionElement.HasExtensions() || (!string.IsNullOrEmpty(current.VersionElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_version",false,current.VersionElement.Extension,current.VersionElement.ElementId);
        }
      }

      if (current.NameElement != null)
      {
        if (!string.IsNullOrEmpty(current.NameElement.Value))
        {
          writer.WriteString("name",current.NameElement.Value);
        }
        if (current.NameElement.HasExtensions() || (!string.IsNullOrEmpty(current.NameElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_name",false,current.NameElement.Extension,current.NameElement.ElementId);
        }
      }

      if (current.TitleElement != null)
      {
        if (!string.IsNullOrEmpty(current.TitleElement.Value))
        {
          writer.WriteString("title",current.TitleElement.Value);
        }
        if (current.TitleElement.HasExtensions() || (!string.IsNullOrEmpty(current.TitleElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_title",false,current.TitleElement.Extension,current.TitleElement.ElementId);
        }
      }

      if ((current.ReplacesElement != null) && (current.ReplacesElement.Count != 0))
      {
        writer.WritePropertyName("replaces");
        writer.WriteStartArray();
        bool foundExtensions = false;
        foreach (Canonical val in current.ReplacesElement)
        {
          if (val.HasExtensions())
          {
            foundExtensions = true;
            break;
          }
        }

        foreach (Canonical val in current.ReplacesElement)
        {
          if (string.IsNullOrEmpty(val.Value))
          {
            if (foundExtensions) { writer.WriteNullValue(); }
          }
          else
          {
            writer.WriteStringValue(val.Value);
          }

        }
        if (foundExtensions)
        {
          writer.WriteEndArray();
          writer.WritePropertyName("_replaces");
          writer.WriteStartArray();
          foreach (Canonical val in current.ReplacesElement)
          {
            if (val.HasExtensions() || (!string.IsNullOrEmpty(val.ElementId)))
            {
              JsonStreamUtilities.SerializeExtensionList(writer,options,string.Empty,true,val.Extension,val.ElementId);
            }
            else
            {
              writer.WriteNullValue();
            }

          }
        }
        writer.WriteEndArray();
      }

      writer.WriteString("status",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.StatusElement.Value));

      if (current.ExperimentalElement != null)
      {
        if (current.ExperimentalElement.Value != null)
        {
          writer.WriteBoolean("experimental",(bool)current.ExperimentalElement.Value);
        }
        if (current.ExperimentalElement.HasExtensions() || (!string.IsNullOrEmpty(current.ExperimentalElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_experimental",false,current.ExperimentalElement.Extension,current.ExperimentalElement.ElementId);
        }
      }

      writer.WriteString("date",current.DateElement.Value);

      if (current.PublisherElement != null)
      {
        if (!string.IsNullOrEmpty(current.PublisherElement.Value))
        {
          writer.WriteString("publisher",current.PublisherElement.Value);
        }
        if (current.PublisherElement.HasExtensions() || (!string.IsNullOrEmpty(current.PublisherElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_publisher",false,current.PublisherElement.Extension,current.PublisherElement.ElementId);
        }
      }

      if ((current.Contact != null) && (current.Contact.Count != 0))
      {
        writer.WritePropertyName("contact");
        writer.WriteStartArray();
        foreach (ContactDetail val in current.Contact)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Description != null)
      {
        if (!string.IsNullOrEmpty(current.Description.Value))
        {
          writer.WriteString("description",current.Description.Value);
        }
        if (current.Description.HasExtensions() || (!string.IsNullOrEmpty(current.Description.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_description",false,current.Description.Extension,current.Description.ElementId);
        }
      }

      if ((current.UseContext != null) && (current.UseContext.Count != 0))
      {
        writer.WritePropertyName("useContext");
        writer.WriteStartArray();
        foreach (UsageContext val in current.UseContext)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Jurisdiction != null) && (current.Jurisdiction.Count != 0))
      {
        writer.WritePropertyName("jurisdiction");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Jurisdiction)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Purpose != null)
      {
        if (!string.IsNullOrEmpty(current.Purpose.Value))
        {
          writer.WriteString("purpose",current.Purpose.Value);
        }
        if (current.Purpose.HasExtensions() || (!string.IsNullOrEmpty(current.Purpose.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_purpose",false,current.Purpose.Extension,current.Purpose.ElementId);
        }
      }

      if (current.Copyright != null)
      {
        if (!string.IsNullOrEmpty(current.Copyright.Value))
        {
          writer.WriteString("copyright",current.Copyright.Value);
        }
        if (current.Copyright.HasExtensions() || (!string.IsNullOrEmpty(current.Copyright.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_copyright",false,current.Copyright.Extension,current.Copyright.ElementId);
        }
      }

      if (current.BaseElement != null)
      {
        if (!string.IsNullOrEmpty(current.BaseElement.Value))
        {
          writer.WriteString("base",current.BaseElement.Value);
        }
        if (current.BaseElement.HasExtensions() || (!string.IsNullOrEmpty(current.BaseElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_base",false,current.BaseElement.Extension,current.BaseElement.ElementId);
        }
      }

      if ((current.ParentElement != null) && (current.ParentElement.Count != 0))
      {
        writer.WritePropertyName("parent");
        writer.WriteStartArray();
        bool foundExtensions = false;
        foreach (Canonical val in current.ParentElement)
        {
          if (val.HasExtensions())
          {
            foundExtensions = true;
            break;
          }
        }

        foreach (Canonical val in current.ParentElement)
        {
          if (string.IsNullOrEmpty(val.Value))
          {
            if (foundExtensions) { writer.WriteNullValue(); }
          }
          else
          {
            writer.WriteStringValue(val.Value);
          }

        }
        if (foundExtensions)
        {
          writer.WriteEndArray();
          writer.WritePropertyName("_parent");
          writer.WriteStartArray();
          foreach (Canonical val in current.ParentElement)
          {
            if (val.HasExtensions() || (!string.IsNullOrEmpty(val.ElementId)))
            {
              JsonStreamUtilities.SerializeExtensionList(writer,options,string.Empty,true,val.Extension,val.ElementId);
            }
            else
            {
              writer.WriteNullValue();
            }

          }
        }
        writer.WriteEndArray();
      }

      if (current.Event != null)
      {
        switch (current.Event)
        {
          case Coding v_Coding:
            writer.WritePropertyName("eventCoding");
            v_Coding.SerializeJson(writer, options);
            break;
          case FhirUri v_FhirUri:
            writer.WriteString("eventUri",v_FhirUri.Value);
            break;
        }
      }
      if (current.CategoryElement != null)
      {
        writer.WriteString("category",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.CategoryElement.Value));
      }

      if ((current.Focus != null) && (current.Focus.Count != 0))
      {
        writer.WritePropertyName("focus");
        writer.WriteStartArray();
        foreach (MessageDefinition.FocusComponent val in current.Focus)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.ResponseRequiredElement != null)
      {
        writer.WriteString("responseRequired",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.ResponseRequiredElement.Value));
      }

      if ((current.AllowedResponse != null) && (current.AllowedResponse.Count != 0))
      {
        writer.WritePropertyName("allowedResponse");
        writer.WriteStartArray();
        foreach (MessageDefinition.AllowedResponseComponent val in current.AllowedResponse)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.GraphElement != null) && (current.GraphElement.Count != 0))
      {
        writer.WritePropertyName("graph");
        writer.WriteStartArray();
        bool foundExtensions = false;
        foreach (Canonical val in current.GraphElement)
        {
          if (val.HasExtensions())
          {
            foundExtensions = true;
            break;
          }
        }

        foreach (Canonical val in current.GraphElement)
        {
          if (string.IsNullOrEmpty(val.Value))
          {
            if (foundExtensions) { writer.WriteNullValue(); }
          }
          else
          {
            writer.WriteStringValue(val.Value);
          }

        }
        if (foundExtensions)
        {
          writer.WriteEndArray();
          writer.WritePropertyName("_graph");
          writer.WriteStartArray();
          foreach (Canonical val in current.GraphElement)
          {
            if (val.HasExtensions() || (!string.IsNullOrEmpty(val.ElementId)))
            {
              JsonStreamUtilities.SerializeExtensionList(writer,options,string.Empty,true,val.Extension,val.ElementId);
            }
            else
            {
              writer.WriteNullValue();
            }

          }
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MessageDefinition
    /// </summary>
    public static void DeserializeJson(this MessageDefinition current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MessageDefinition
    /// </summary>
    public static void DeserializeJsonProperty(this MessageDefinition current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "url":
          current.UrlElement = new FhirUri(reader.GetString());
          break;

        case "_url":
          ((Hl7.Fhir.Model.Element)current.UrlElement).DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Identifier v_Identifier = new Hl7.Fhir.Model.Identifier();
            v_Identifier.DeserializeJson(ref reader, options);
            current.Identifier.Add(v_Identifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Identifier.Count == 0)
          {
            current.Identifier = null;
          }
          break;

        case "version":
          current.VersionElement = new FhirString(reader.GetString());
          break;

        case "_version":
          ((Hl7.Fhir.Model.Element)current.VersionElement).DeserializeJson(ref reader, options);
          break;

        case "name":
          current.NameElement = new FhirString(reader.GetString());
          break;

        case "_name":
          ((Hl7.Fhir.Model.Element)current.NameElement).DeserializeJson(ref reader, options);
          break;

        case "title":
          current.TitleElement = new FhirString(reader.GetString());
          break;

        case "_title":
          ((Hl7.Fhir.Model.Element)current.TitleElement).DeserializeJson(ref reader, options);
          break;

        case "replaces":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.ReplacesElement = new List<Canonical>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            current.ReplacesElement.Add(new Canonical(reader.GetString()));

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.ReplacesElement.Count == 0)
          {
            current.ReplacesElement = null;
          }
          break;

        case "_replaces":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          int i_replaces = 0;

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            if (i_replaces >= current.ReplacesElement.Count)
            {
              current.ReplacesElement.Add(new Canonical());
            }
            ((Hl7.Fhir.Model.Element)current.ReplacesElement[i_replaces++]).DeserializeJson(ref reader, options);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }
          break;

        case "status":
          current.StatusElement =new Code<Hl7.Fhir.Model.PublicationStatus>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.PublicationStatus>(reader.GetString()));
          break;

        case "_status":
          ((Hl7.Fhir.Model.Element)current.StatusElement).DeserializeJson(ref reader, options);
          break;

        case "experimental":
          current.ExperimentalElement = new FhirBoolean(reader.GetBoolean());
          break;

        case "_experimental":
          ((Hl7.Fhir.Model.Element)current.ExperimentalElement).DeserializeJson(ref reader, options);
          break;

        case "date":
          current.DateElement = new FhirDateTime(reader.GetString());
          break;

        case "_date":
          ((Hl7.Fhir.Model.Element)current.DateElement).DeserializeJson(ref reader, options);
          break;

        case "publisher":
          current.PublisherElement = new FhirString(reader.GetString());
          break;

        case "_publisher":
          ((Hl7.Fhir.Model.Element)current.PublisherElement).DeserializeJson(ref reader, options);
          break;

        case "contact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Contact = new List<ContactDetail>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ContactDetail v_Contact = new Hl7.Fhir.Model.ContactDetail();
            v_Contact.DeserializeJson(ref reader, options);
            current.Contact.Add(v_Contact);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Contact.Count == 0)
          {
            current.Contact = null;
          }
          break;

        case "description":
          current.Description = new Markdown(reader.GetString());
          break;

        case "useContext":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.UseContext = new List<UsageContext>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.UsageContext v_UseContext = new Hl7.Fhir.Model.UsageContext();
            v_UseContext.DeserializeJson(ref reader, options);
            current.UseContext.Add(v_UseContext);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.UseContext.Count == 0)
          {
            current.UseContext = null;
          }
          break;

        case "jurisdiction":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Jurisdiction = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Jurisdiction = new Hl7.Fhir.Model.CodeableConcept();
            v_Jurisdiction.DeserializeJson(ref reader, options);
            current.Jurisdiction.Add(v_Jurisdiction);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Jurisdiction.Count == 0)
          {
            current.Jurisdiction = null;
          }
          break;

        case "purpose":
          current.Purpose = new Markdown(reader.GetString());
          break;

        case "copyright":
          current.Copyright = new Markdown(reader.GetString());
          break;

        case "base":
          current.BaseElement = new Canonical(reader.GetString());
          break;

        case "_base":
          ((Hl7.Fhir.Model.Element)current.BaseElement).DeserializeJson(ref reader, options);
          break;

        case "parent":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.ParentElement = new List<Canonical>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            current.ParentElement.Add(new Canonical(reader.GetString()));

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.ParentElement.Count == 0)
          {
            current.ParentElement = null;
          }
          break;

        case "_parent":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          int i_parent = 0;

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            if (i_parent >= current.ParentElement.Count)
            {
              current.ParentElement.Add(new Canonical());
            }
            ((Hl7.Fhir.Model.Element)current.ParentElement[i_parent++]).DeserializeJson(ref reader, options);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }
          break;

        case "eventCoding":
          current.Event = new Hl7.Fhir.Model.Coding();
          ((Hl7.Fhir.Model.Coding)current.Event).DeserializeJson(ref reader, options);
          break;

        case "eventUri":
          current.Event = new FhirUri(reader.GetString());
          break;

        case "category":
          current.CategoryElement =new Code<Hl7.Fhir.Model.MessageDefinition.MessageSignificanceCategory>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.MessageDefinition.MessageSignificanceCategory>(reader.GetString()));
          break;

        case "_category":
          ((Hl7.Fhir.Model.Element)current.CategoryElement).DeserializeJson(ref reader, options);
          break;

        case "focus":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Focus = new List<MessageDefinition.FocusComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.MessageDefinition.FocusComponent v_Focus = new Hl7.Fhir.Model.MessageDefinition.FocusComponent();
            v_Focus.DeserializeJson(ref reader, options);
            current.Focus.Add(v_Focus);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Focus.Count == 0)
          {
            current.Focus = null;
          }
          break;

        case "responseRequired":
          current.ResponseRequiredElement =new Code<Hl7.Fhir.Model.messageheader_response_request>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.messageheader_response_request>(reader.GetString()));
          break;

        case "_responseRequired":
          ((Hl7.Fhir.Model.Element)current.ResponseRequiredElement).DeserializeJson(ref reader, options);
          break;

        case "allowedResponse":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.AllowedResponse = new List<MessageDefinition.AllowedResponseComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.MessageDefinition.AllowedResponseComponent v_AllowedResponse = new Hl7.Fhir.Model.MessageDefinition.AllowedResponseComponent();
            v_AllowedResponse.DeserializeJson(ref reader, options);
            current.AllowedResponse.Add(v_AllowedResponse);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.AllowedResponse.Count == 0)
          {
            current.AllowedResponse = null;
          }
          break;

        case "graph":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.GraphElement = new List<Canonical>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            current.GraphElement.Add(new Canonical(reader.GetString()));

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.GraphElement.Count == 0)
          {
            current.GraphElement = null;
          }
          break;

        case "_graph":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          int i_graph = 0;

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            if (i_graph >= current.GraphElement.Count)
            {
              current.GraphElement.Add(new Canonical());
            }
            ((Hl7.Fhir.Model.Element)current.GraphElement[i_graph++]).DeserializeJson(ref reader, options);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }
          break;

        // Complex: MessageDefinition, Export: MessageDefinition, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR MessageDefinition#Focus into JSON
    /// </summary>
    public static void SerializeJson(this MessageDefinition.FocusComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: MessageDefinition#Focus, Export: FocusComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      writer.WriteString("code",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.CodeElement.Value));

      if (current.ProfileElement != null)
      {
        if (!string.IsNullOrEmpty(current.ProfileElement.Value))
        {
          writer.WriteString("profile",current.ProfileElement.Value);
        }
        if (current.ProfileElement.HasExtensions() || (!string.IsNullOrEmpty(current.ProfileElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_profile",false,current.ProfileElement.Extension,current.ProfileElement.ElementId);
        }
      }

      writer.WriteNumber("min",(int)current.MinElement.Value);

      if (current.MaxElement != null)
      {
        if (!string.IsNullOrEmpty(current.MaxElement.Value))
        {
          writer.WriteString("max",current.MaxElement.Value);
        }
        if (current.MaxElement.HasExtensions() || (!string.IsNullOrEmpty(current.MaxElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_max",false,current.MaxElement.Extension,current.MaxElement.ElementId);
        }
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MessageDefinition#Focus
    /// </summary>
    public static void DeserializeJson(this MessageDefinition.FocusComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MessageDefinition#Focus
    /// </summary>
    public static void DeserializeJsonProperty(this MessageDefinition.FocusComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "code":
          current.CodeElement =new Code<Hl7.Fhir.Model.ResourceType>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.ResourceType>(reader.GetString()));
          break;

        case "_code":
          ((Hl7.Fhir.Model.Element)current.CodeElement).DeserializeJson(ref reader, options);
          break;

        case "profile":
          current.ProfileElement = new Canonical(reader.GetString());
          break;

        case "_profile":
          ((Hl7.Fhir.Model.Element)current.ProfileElement).DeserializeJson(ref reader, options);
          break;

        case "min":
          current.MinElement = new UnsignedInt(reader.GetInt32());
          break;

        case "_min":
          ((Hl7.Fhir.Model.Element)current.MinElement).DeserializeJson(ref reader, options);
          break;

        case "max":
          current.MaxElement = new FhirString(reader.GetString());
          break;

        case "_max":
          ((Hl7.Fhir.Model.Element)current.MaxElement).DeserializeJson(ref reader, options);
          break;

        // Complex: focus, Export: FocusComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR MessageDefinition#AllowedResponse into JSON
    /// </summary>
    public static void SerializeJson(this MessageDefinition.AllowedResponseComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: MessageDefinition#AllowedResponse, Export: AllowedResponseComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      writer.WriteString("message",current.MessageElement.Value);

      if (current.Situation != null)
      {
        if (!string.IsNullOrEmpty(current.Situation.Value))
        {
          writer.WriteString("situation",current.Situation.Value);
        }
        if (current.Situation.HasExtensions() || (!string.IsNullOrEmpty(current.Situation.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_situation",false,current.Situation.Extension,current.Situation.ElementId);
        }
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MessageDefinition#AllowedResponse
    /// </summary>
    public static void DeserializeJson(this MessageDefinition.AllowedResponseComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MessageDefinition#AllowedResponse
    /// </summary>
    public static void DeserializeJsonProperty(this MessageDefinition.AllowedResponseComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "message":
          current.MessageElement = new Canonical(reader.GetString());
          break;

        case "_message":
          ((Hl7.Fhir.Model.Element)current.MessageElement).DeserializeJson(ref reader, options);
          break;

        case "situation":
          current.Situation = new Markdown(reader.GetString());
          break;

        // Complex: allowedResponse, Export: AllowedResponseComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class MessageDefinitionJsonConverter : JsonConverter<MessageDefinition>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(MessageDefinition).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, MessageDefinition value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override MessageDefinition Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        MessageDefinition target = new MessageDefinition();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
