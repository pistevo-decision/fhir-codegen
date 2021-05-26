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
  /// JSON Serialization Extensions for Address
  /// </summary>
  public static class AddressJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR Address into JSON
    /// </summary>
    public static void SerializeJson(this Address current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      if (current.UseElement != null)
      {
        writer.WriteString("use",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.UseElement.Value));
      }

      if (current.TypeElement != null)
      {
        writer.WriteString("type",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.TypeElement.Value));
      }

      if (current.TextElement != null)
      {
        if (!string.IsNullOrEmpty(current.TextElement.Value))
        {
          writer.WriteString("text",current.TextElement.Value);
        }
        if (current.TextElement.HasExtensions() || (!string.IsNullOrEmpty(current.TextElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_text",false,current.TextElement.Extension,current.TextElement.ElementId);
        }
      }

      if ((current.LineElement != null) && (current.LineElement.Count != 0))
      {
        writer.WritePropertyName("line");
        writer.WriteStartArray();
        bool foundExtensions = false;
        foreach (FhirString val in current.LineElement)
        {
          if (val.HasExtensions())
          {
            foundExtensions = true;
            break;
          }
        }

        foreach (FhirString val in current.LineElement)
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
          writer.WritePropertyName("_line");
          writer.WriteStartArray();
          foreach (FhirString val in current.LineElement)
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

      if (current.CityElement != null)
      {
        if (!string.IsNullOrEmpty(current.CityElement.Value))
        {
          writer.WriteString("city",current.CityElement.Value);
        }
        if (current.CityElement.HasExtensions() || (!string.IsNullOrEmpty(current.CityElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_city",false,current.CityElement.Extension,current.CityElement.ElementId);
        }
      }

      if (current.DistrictElement != null)
      {
        if (!string.IsNullOrEmpty(current.DistrictElement.Value))
        {
          writer.WriteString("district",current.DistrictElement.Value);
        }
        if (current.DistrictElement.HasExtensions() || (!string.IsNullOrEmpty(current.DistrictElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_district",false,current.DistrictElement.Extension,current.DistrictElement.ElementId);
        }
      }

      if (current.StateElement != null)
      {
        if (!string.IsNullOrEmpty(current.StateElement.Value))
        {
          writer.WriteString("state",current.StateElement.Value);
        }
        if (current.StateElement.HasExtensions() || (!string.IsNullOrEmpty(current.StateElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_state",false,current.StateElement.Extension,current.StateElement.ElementId);
        }
      }

      if (current.PostalCodeElement != null)
      {
        if (!string.IsNullOrEmpty(current.PostalCodeElement.Value))
        {
          writer.WriteString("postalCode",current.PostalCodeElement.Value);
        }
        if (current.PostalCodeElement.HasExtensions() || (!string.IsNullOrEmpty(current.PostalCodeElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_postalCode",false,current.PostalCodeElement.Extension,current.PostalCodeElement.ElementId);
        }
      }

      if (current.CountryElement != null)
      {
        if (!string.IsNullOrEmpty(current.CountryElement.Value))
        {
          writer.WriteString("country",current.CountryElement.Value);
        }
        if (current.CountryElement.HasExtensions() || (!string.IsNullOrEmpty(current.CountryElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_country",false,current.CountryElement.Extension,current.CountryElement.ElementId);
        }
      }

      if (current.Period != null)
      {
        writer.WritePropertyName("period");
        current.Period.SerializeJson(writer, options);
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Address
    /// </summary>
    public static void DeserializeJson(this Address current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR Address
    /// </summary>
    public static void DeserializeJsonProperty(this Address current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "use":
          current.UseElement =new Code<Hl7.Fhir.Model.Address.AddressUse>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.Address.AddressUse>(reader.GetString()));
          break;

        case "_use":
          ((Hl7.Fhir.Model.Element)current.UseElement).DeserializeJson(ref reader, options);
          break;

        case "type":
          current.TypeElement =new Code<Hl7.Fhir.Model.Address.AddressType>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.Address.AddressType>(reader.GetString()));
          break;

        case "_type":
          ((Hl7.Fhir.Model.Element)current.TypeElement).DeserializeJson(ref reader, options);
          break;

        case "text":
          current.TextElement = new FhirString(reader.GetString());
          break;

        case "_text":
          ((Hl7.Fhir.Model.Element)current.TextElement).DeserializeJson(ref reader, options);
          break;

        case "line":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.LineElement = new List<FhirString>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            current.LineElement.Add(new FhirString(reader.GetString()));

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.LineElement.Count == 0)
          {
            current.LineElement = null;
          }
          break;

        case "_line":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          int i_line = 0;

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            if (i_line >= current.LineElement.Count)
            {
              current.LineElement.Add(new FhirString());
            }
            ((Hl7.Fhir.Model.Element)current.LineElement[i_line++]).DeserializeJson(ref reader, options);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }
          break;

        case "city":
          current.CityElement = new FhirString(reader.GetString());
          break;

        case "_city":
          ((Hl7.Fhir.Model.Element)current.CityElement).DeserializeJson(ref reader, options);
          break;

        case "district":
          current.DistrictElement = new FhirString(reader.GetString());
          break;

        case "_district":
          ((Hl7.Fhir.Model.Element)current.DistrictElement).DeserializeJson(ref reader, options);
          break;

        case "state":
          current.StateElement = new FhirString(reader.GetString());
          break;

        case "_state":
          ((Hl7.Fhir.Model.Element)current.StateElement).DeserializeJson(ref reader, options);
          break;

        case "postalCode":
          current.PostalCodeElement = new FhirString(reader.GetString());
          break;

        case "_postalCode":
          ((Hl7.Fhir.Model.Element)current.PostalCodeElement).DeserializeJson(ref reader, options);
          break;

        case "country":
          current.CountryElement = new FhirString(reader.GetString());
          break;

        case "_country":
          ((Hl7.Fhir.Model.Element)current.CountryElement).DeserializeJson(ref reader, options);
          break;

        case "period":
          current.Period = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.Period).DeserializeJson(ref reader, options);
          break;

      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class AddressJsonConverter : JsonConverter<Address>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(Address).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, Address value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override Address Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        Address target = new Address();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
