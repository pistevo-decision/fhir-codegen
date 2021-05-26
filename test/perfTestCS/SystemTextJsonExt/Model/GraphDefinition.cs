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
  /// JSON Serialization Extensions for GraphDefinition
  /// </summary>
  public static class GraphDefinitionJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR GraphDefinition into JSON
    /// </summary>
    public static void SerializeJson(this GraphDefinition current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","GraphDefinition");
      // Complex: GraphDefinition, Export: GraphDefinition, Base: DomainResource (DomainResource)
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

      writer.WriteString("name",current.NameElement.Value);

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

      if (current.DateElement != null)
      {
        if (!string.IsNullOrEmpty(current.DateElement.Value))
        {
          writer.WriteString("date",current.DateElement.Value);
        }
        if (current.DateElement.HasExtensions() || (!string.IsNullOrEmpty(current.DateElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_date",false,current.DateElement.Extension,current.DateElement.ElementId);
        }
      }

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

      writer.WriteString("start",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.StartElement.Value));

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

      if ((current.Link != null) && (current.Link.Count != 0))
      {
        writer.WritePropertyName("link");
        writer.WriteStartArray();
        foreach (GraphDefinition.LinkComponent val in current.Link)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR GraphDefinition
    /// </summary>
    public static void DeserializeJson(this GraphDefinition current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR GraphDefinition
    /// </summary>
    public static void DeserializeJsonProperty(this GraphDefinition current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "url":
          current.UrlElement = new FhirUri(reader.GetString());
          break;

        case "_url":
          ((Hl7.Fhir.Model.Element)current.UrlElement).DeserializeJson(ref reader, options);
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

        case "start":
          current.StartElement =new Code<Hl7.Fhir.Model.ResourceType>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.ResourceType>(reader.GetString()));
          break;

        case "_start":
          ((Hl7.Fhir.Model.Element)current.StartElement).DeserializeJson(ref reader, options);
          break;

        case "profile":
          current.ProfileElement = new Canonical(reader.GetString());
          break;

        case "_profile":
          ((Hl7.Fhir.Model.Element)current.ProfileElement).DeserializeJson(ref reader, options);
          break;

        case "link":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Link = new List<GraphDefinition.LinkComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.GraphDefinition.LinkComponent v_Link = new Hl7.Fhir.Model.GraphDefinition.LinkComponent();
            v_Link.DeserializeJson(ref reader, options);
            current.Link.Add(v_Link);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Link.Count == 0)
          {
            current.Link = null;
          }
          break;

        // Complex: GraphDefinition, Export: GraphDefinition, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR GraphDefinition#Link into JSON
    /// </summary>
    public static void SerializeJson(this GraphDefinition.LinkComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: GraphDefinition#Link, Export: LinkComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.PathElement != null)
      {
        if (!string.IsNullOrEmpty(current.PathElement.Value))
        {
          writer.WriteString("path",current.PathElement.Value);
        }
        if (current.PathElement.HasExtensions() || (!string.IsNullOrEmpty(current.PathElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_path",false,current.PathElement.Extension,current.PathElement.ElementId);
        }
      }

      if (current.SliceNameElement != null)
      {
        if (!string.IsNullOrEmpty(current.SliceNameElement.Value))
        {
          writer.WriteString("sliceName",current.SliceNameElement.Value);
        }
        if (current.SliceNameElement.HasExtensions() || (!string.IsNullOrEmpty(current.SliceNameElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_sliceName",false,current.SliceNameElement.Extension,current.SliceNameElement.ElementId);
        }
      }

      if (current.MinElement != null)
      {
        if (current.MinElement.Value != null)
        {
          writer.WriteNumber("min",(int)current.MinElement.Value);
        }
        if (current.MinElement.HasExtensions() || (!string.IsNullOrEmpty(current.MinElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_min",false,current.MinElement.Extension,current.MinElement.ElementId);
        }
      }

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

      if (current.DescriptionElement != null)
      {
        if (!string.IsNullOrEmpty(current.DescriptionElement.Value))
        {
          writer.WriteString("description",current.DescriptionElement.Value);
        }
        if (current.DescriptionElement.HasExtensions() || (!string.IsNullOrEmpty(current.DescriptionElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_description",false,current.DescriptionElement.Extension,current.DescriptionElement.ElementId);
        }
      }

      if ((current.Target != null) && (current.Target.Count != 0))
      {
        writer.WritePropertyName("target");
        writer.WriteStartArray();
        foreach (GraphDefinition.TargetComponent val in current.Target)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR GraphDefinition#Link
    /// </summary>
    public static void DeserializeJson(this GraphDefinition.LinkComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR GraphDefinition#Link
    /// </summary>
    public static void DeserializeJsonProperty(this GraphDefinition.LinkComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "path":
          current.PathElement = new FhirString(reader.GetString());
          break;

        case "_path":
          ((Hl7.Fhir.Model.Element)current.PathElement).DeserializeJson(ref reader, options);
          break;

        case "sliceName":
          current.SliceNameElement = new FhirString(reader.GetString());
          break;

        case "_sliceName":
          ((Hl7.Fhir.Model.Element)current.SliceNameElement).DeserializeJson(ref reader, options);
          break;

        case "min":
          current.MinElement = new Integer(reader.GetInt32());
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

        case "description":
          current.DescriptionElement = new FhirString(reader.GetString());
          break;

        case "_description":
          ((Hl7.Fhir.Model.Element)current.DescriptionElement).DeserializeJson(ref reader, options);
          break;

        case "target":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Target = new List<GraphDefinition.TargetComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.GraphDefinition.TargetComponent v_Target = new Hl7.Fhir.Model.GraphDefinition.TargetComponent();
            v_Target.DeserializeJson(ref reader, options);
            current.Target.Add(v_Target);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Target.Count == 0)
          {
            current.Target = null;
          }
          break;

        // Complex: link, Export: LinkComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR GraphDefinition#Target into JSON
    /// </summary>
    public static void SerializeJson(this GraphDefinition.TargetComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: GraphDefinition#Target, Export: TargetComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      writer.WriteString("type",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.TypeElement.Value));

      if (current.ParamsElement != null)
      {
        if (!string.IsNullOrEmpty(current.ParamsElement.Value))
        {
          writer.WriteString("params",current.ParamsElement.Value);
        }
        if (current.ParamsElement.HasExtensions() || (!string.IsNullOrEmpty(current.ParamsElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_params",false,current.ParamsElement.Extension,current.ParamsElement.ElementId);
        }
      }

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

      if ((current.Compartment != null) && (current.Compartment.Count != 0))
      {
        writer.WritePropertyName("compartment");
        writer.WriteStartArray();
        foreach (GraphDefinition.CompartmentComponent val in current.Compartment)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Link != null) && (current.Link.Count != 0))
      {
        writer.WritePropertyName("link");
        writer.WriteStartArray();
        foreach (GraphDefinition.LinkComponent val in current.Link)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR GraphDefinition#Target
    /// </summary>
    public static void DeserializeJson(this GraphDefinition.TargetComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR GraphDefinition#Target
    /// </summary>
    public static void DeserializeJsonProperty(this GraphDefinition.TargetComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "type":
          current.TypeElement =new Code<Hl7.Fhir.Model.ResourceType>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.ResourceType>(reader.GetString()));
          break;

        case "_type":
          ((Hl7.Fhir.Model.Element)current.TypeElement).DeserializeJson(ref reader, options);
          break;

        case "params":
          current.ParamsElement = new FhirString(reader.GetString());
          break;

        case "_params":
          ((Hl7.Fhir.Model.Element)current.ParamsElement).DeserializeJson(ref reader, options);
          break;

        case "profile":
          current.ProfileElement = new Canonical(reader.GetString());
          break;

        case "_profile":
          ((Hl7.Fhir.Model.Element)current.ProfileElement).DeserializeJson(ref reader, options);
          break;

        case "compartment":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Compartment = new List<GraphDefinition.CompartmentComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.GraphDefinition.CompartmentComponent v_Compartment = new Hl7.Fhir.Model.GraphDefinition.CompartmentComponent();
            v_Compartment.DeserializeJson(ref reader, options);
            current.Compartment.Add(v_Compartment);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Compartment.Count == 0)
          {
            current.Compartment = null;
          }
          break;

        case "link":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Link = new List<GraphDefinition.LinkComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.GraphDefinition.LinkComponent v_Link = new Hl7.Fhir.Model.GraphDefinition.LinkComponent();
            v_Link.DeserializeJson(ref reader, options);
            current.Link.Add(v_Link);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Link.Count == 0)
          {
            current.Link = null;
          }
          break;

        // Complex: target, Export: TargetComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR GraphDefinition#Compartment into JSON
    /// </summary>
    public static void SerializeJson(this GraphDefinition.CompartmentComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: GraphDefinition#Compartment, Export: CompartmentComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      writer.WriteString("use",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.UseElement.Value));

      writer.WriteString("code",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.CodeElement.Value));

      writer.WriteString("rule",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.RuleElement.Value));

      if (current.ExpressionElement != null)
      {
        if (!string.IsNullOrEmpty(current.ExpressionElement.Value))
        {
          writer.WriteString("expression",current.ExpressionElement.Value);
        }
        if (current.ExpressionElement.HasExtensions() || (!string.IsNullOrEmpty(current.ExpressionElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_expression",false,current.ExpressionElement.Extension,current.ExpressionElement.ElementId);
        }
      }

      if (current.DescriptionElement != null)
      {
        if (!string.IsNullOrEmpty(current.DescriptionElement.Value))
        {
          writer.WriteString("description",current.DescriptionElement.Value);
        }
        if (current.DescriptionElement.HasExtensions() || (!string.IsNullOrEmpty(current.DescriptionElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_description",false,current.DescriptionElement.Extension,current.DescriptionElement.ElementId);
        }
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR GraphDefinition#Compartment
    /// </summary>
    public static void DeserializeJson(this GraphDefinition.CompartmentComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR GraphDefinition#Compartment
    /// </summary>
    public static void DeserializeJsonProperty(this GraphDefinition.CompartmentComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "use":
          current.UseElement =new Code<Hl7.Fhir.Model.GraphDefinition.GraphCompartmentUse>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.GraphDefinition.GraphCompartmentUse>(reader.GetString()));
          break;

        case "_use":
          ((Hl7.Fhir.Model.Element)current.UseElement).DeserializeJson(ref reader, options);
          break;

        case "code":
          current.CodeElement =new Code<Hl7.Fhir.Model.CompartmentType>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.CompartmentType>(reader.GetString()));
          break;

        case "_code":
          ((Hl7.Fhir.Model.Element)current.CodeElement).DeserializeJson(ref reader, options);
          break;

        case "rule":
          current.RuleElement =new Code<Hl7.Fhir.Model.GraphDefinition.GraphCompartmentRule>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.GraphDefinition.GraphCompartmentRule>(reader.GetString()));
          break;

        case "_rule":
          ((Hl7.Fhir.Model.Element)current.RuleElement).DeserializeJson(ref reader, options);
          break;

        case "expression":
          current.ExpressionElement = new FhirString(reader.GetString());
          break;

        case "_expression":
          ((Hl7.Fhir.Model.Element)current.ExpressionElement).DeserializeJson(ref reader, options);
          break;

        case "description":
          current.DescriptionElement = new FhirString(reader.GetString());
          break;

        case "_description":
          ((Hl7.Fhir.Model.Element)current.DescriptionElement).DeserializeJson(ref reader, options);
          break;

        // Complex: compartment, Export: CompartmentComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class GraphDefinitionJsonConverter : JsonConverter<GraphDefinition>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(GraphDefinition).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, GraphDefinition value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override GraphDefinition Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        GraphDefinition target = new GraphDefinition();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
