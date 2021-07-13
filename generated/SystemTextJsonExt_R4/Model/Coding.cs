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
  /// JSON Serialization Extensions for Coding
  /// </summary>
  public static class CodingJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR Coding into JSON
    /// </summary>
    public static void SerializeJson(this Coding current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Complex: Coding, Export: Coding, Base: Element (Element)
      ((Hl7.Fhir.Model.Element)current).SerializeJson(writer, options, false);

      if (current.SystemElement != null)
      {
        if (!string.IsNullOrEmpty(current.SystemElement.Value))
        {
          writer.WriteString("system",current.SystemElement.Value);
        }
        if (current.SystemElement.HasExtensions() || (!string.IsNullOrEmpty(current.SystemElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_system",false,current.SystemElement.Extension,current.SystemElement.ElementId);
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

      if (current.CodeElement != null)
      {
        if (!string.IsNullOrEmpty(current.CodeElement.Value))
        {
          writer.WriteString("code",current.CodeElement.Value.Trim());
        }
        if (current.CodeElement.HasExtensions() || (!string.IsNullOrEmpty(current.CodeElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_code",false,current.CodeElement.Extension,current.CodeElement.ElementId);
        }
      }

      if (current.DisplayElement != null)
      {
        if (!string.IsNullOrEmpty(current.DisplayElement.Value))
        {
          writer.WriteString("display",current.DisplayElement.Value);
        }
        if (current.DisplayElement.HasExtensions() || (!string.IsNullOrEmpty(current.DisplayElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_display",false,current.DisplayElement.Extension,current.DisplayElement.ElementId);
        }
      }

      if (current.UserSelectedElement != null)
      {
        if (current.UserSelectedElement.Value != null)
        {
          writer.WriteBoolean("userSelected",(bool)current.UserSelectedElement.Value);
        }
        if (current.UserSelectedElement.HasExtensions() || (!string.IsNullOrEmpty(current.UserSelectedElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_userSelected",false,current.UserSelectedElement.Extension,current.UserSelectedElement.ElementId);
        }
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Coding
    /// </summary>
    public static void DeserializeJson(this Coding current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          if (Hl7.Fhir.Serialization.FhirSerializerOptions.Debug) { Console.WriteLine($"Coding >>> Coding.{propertyName}, depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}"); }
          reader.Read();
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException($"Coding: invalid state! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Coding
    /// </summary>
    public static void DeserializeJsonProperty(this Coding current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "system":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.SystemElement = new FhirUri();
            reader.Skip();
          }
          else
          {
            current.SystemElement = new FhirUri(reader.GetString());
          }
          break;

        case "_system":
          if (current.SystemElement == null) { current.SystemElement = new FhirUri(); }
          ((Hl7.Fhir.Model.Element)current.SystemElement).DeserializeJson(ref reader, options);
          break;

        case "version":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.VersionElement = new FhirString();
            reader.Skip();
          }
          else
          {
            current.VersionElement = new FhirString(reader.GetString());
          }
          break;

        case "_version":
          if (current.VersionElement == null) { current.VersionElement = new FhirString(); }
          ((Hl7.Fhir.Model.Element)current.VersionElement).DeserializeJson(ref reader, options);
          break;

        case "code":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.CodeElement = new Code();
            reader.Skip();
          }
          else
          {
            current.CodeElement = new Code(reader.GetString());
          }
          break;

        case "_code":
          if (current.CodeElement == null) { current.CodeElement = new Code(); }
          ((Hl7.Fhir.Model.Element)current.CodeElement).DeserializeJson(ref reader, options);
          break;

        case "display":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.DisplayElement = new FhirString();
            reader.Skip();
          }
          else
          {
            current.DisplayElement = new FhirString(reader.GetString());
          }
          break;

        case "_display":
          if (current.DisplayElement == null) { current.DisplayElement = new FhirString(); }
          ((Hl7.Fhir.Model.Element)current.DisplayElement).DeserializeJson(ref reader, options);
          break;

        case "userSelected":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.UserSelectedElement = new FhirBoolean();
            reader.Skip();
          }
          else
          {
            current.UserSelectedElement = new FhirBoolean(reader.GetBoolean());
          }
          break;

        case "_userSelected":
          if (current.UserSelectedElement == null) { current.UserSelectedElement = new FhirBoolean(); }
          ((Hl7.Fhir.Model.Element)current.UserSelectedElement).DeserializeJson(ref reader, options);
          break;

        // Complex: Coding, Export: Coding, Base: Element
        default:
          ((Hl7.Fhir.Model.Element)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class CodingJsonConverter : JsonConverter<Coding>
    {
      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, Coding value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override Coding Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        Coding target = new Coding();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file