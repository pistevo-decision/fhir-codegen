// <copyright file="Annotation.cs" company="Microsoft Corporation">
//     Copyright (c) Microsoft Corporation. All rights reserved.
//     Licensed under the MIT License (MIT). See LICENSE in the repo root for license information.
// </copyright>

using Hl7.Fhir.ElementModel;
using Hl7.Fhir.Model;

namespace Microsoft.Health.Fhir.CrossVersion.Convert_20_50;

public class Annotation_20_50 : ICrossVersionProcessor<Annotation>, ICrossVersionExtractor<Annotation>
{
	private Converter_20_50 _converter;
	internal Annotation_20_50(Converter_20_50 converter)
	{
		_converter = converter;
	}

	public Annotation Extract(ISourceNode node)
	{
		Annotation v = new();
		foreach (ISourceNode child in node.Children())
		{
			Process(child, v);
		}

		return v;
	}

	public void Process(ISourceNode node, Annotation current)
	{
		switch (node.Name)
		{
			case "authorReference":
				current.Author = new ResourceReference(node.Text);
				break;

			case "authorString":
				current.Author = new FhirString(node.Text);
				break;

			case "_authorString":
				_converter._element.Process(node, current.Author);
				break;

			case "time":
				current.TimeElement = new FhirDateTime(node.Text);
				break;

			case "_time":
				_converter._element.Process(node, current.TimeElement);
				break;

			case "text":
				current.TextElement = new Markdown(node.Text);
				break;

			// process inherited elements
			default:
				_converter._element.Process(node, current);
				break;

		}
	}
}
