#include <cstddef>
#include <deque>
#include <list>
#include <vector>

#include <glm/gtc/type_precision.hpp>

#include "AttributeSet.hpp"
#include "json.hpp"
#include "profile_timer.hpp"
#include "SDLWrapper.hpp"
#include "CameraObject.hpp"
#include "Canvas.hpp"
#include "Font.hpp"
#include "LightObject.hpp"
#include "ModelMatrixScope.hpp"
#include "ParticleSystem.hpp"
#include "Renderable.hpp"
#include "RenderManager.hpp"
#include "RenderQueue.hpp"
#include "RenderTarget.hpp"
#include "SceneGraph.hpp"
#include "SceneNode.hpp"
#include "Shaders.hpp"
#include "Surface.hpp"
#include "TexPack.hpp"
#include "UniformBuffer.hpp"
#include "WindowManager.hpp"
#include "VGraph.hpp"

#include "variant_utils.hpp"

#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

namespace
{	
	struct vertex_color
	{
		vertex_color() : vertex(0.0f), color(0) {}
		vertex_color(const glm::vec2& v, const glm::u8vec4& c) : vertex(v), color(c) {}
		glm::vec2 vertex;
		glm::u8vec4 color;
	};

	class SquareRenderable : public KRE::SceneObject
	{
	public:
		SquareRenderable() : KRE::SceneObject("square") {
			using namespace KRE;

			setShader(ShaderProgram::getProgram("attr_color_shader"));

			auto ab = DisplayDevice::createAttributeSet(false, false, false);
			auto pc = new Attribute<vertex_color>(AccessFreqHint::DYNAMIC, AccessTypeHint::DRAW);
			pc->addAttributeDesc(AttributeDesc(AttrType::POSITION, 2, AttrFormat::FLOAT, false, sizeof(vertex_color), offsetof(vertex_color, vertex)));
			pc->addAttributeDesc(AttributeDesc(AttrType::COLOR, 4, AttrFormat::UNSIGNED_BYTE, true, sizeof(vertex_color), offsetof(vertex_color, color)));
			ab->addAttribute(AttributeBasePtr(pc));
			ab->setDrawMode(DrawMode::TRIANGLE_STRIP);
			addAttributeSet(ab);

			std::vector<vertex_color> vertices;
			vertices.emplace_back(glm::vec2(0.0f,0.0f), glm::u8vec4(255,0,0,255));
			vertices.emplace_back(glm::vec2(0.0f,100.0f), glm::u8vec4(0,255,0,255));
			vertices.emplace_back(glm::vec2(100.0f,0.0f), glm::u8vec4(0,0,255,255));
			vertices.emplace_back(glm::vec2(100.0f,100.0f), glm::u8vec4(255,0,0,255));
			ab->setCount(vertices.size());
			pc->update(&vertices, pc->end());

			//std::vector<uint8_t> indices;
			//indices.emplace_back(0);
			//indices.emplace_back(1);
			//indices.emplace_back(2);
			//indices.emplace_back(3);
			//ab->UpdateIndicies(indices);

			//pc->Update(vertices, pc->begin()+5);

			/*
			// first parameter is a hint to wether the buffer should be hardware backed.
			// second parameter is whether we are using indexed drawing.
			// third parameter is wether this is instanced.
			auto ab = DisplayDevice::CreateAttributeSet(false, true, false);
			// If these were instanced then there is an extra parameter on the end, which defaults to 1.
			// A 0 indictes that there is no advancement per instance
			auto pos = ab->CreateAttribute();
			pos->AddAttributeDescription(AttributeDesc(AttributeDesc::Type::POSITION, 2, AttributeDesc::VariableType::FLOAT, false));
			auto col = ab->CreateAttribute();
			col->AddAttributeDescription(AttributeDesc(AttributeDesc::Type::COLOR, 4, AttributeDesc::VariableType::UNSIGNED_BYTE, true));
			ab->SetDrawMode(AttributeSet::DrawMode::TRIANGLES);
			AddAttributeSet(ab);

			std::vector<glm::vec2> vertices;
			vertices.emplace_back(0.0f, 0.0f);
			vertices.emplace_back(0.0f, 1.0f);
			vertices.emplace_back(1.0f, 0.0f);
			vertices.emplace_back(1.0f, 1.0f);
			//ab->SetCount(vertices.size());
			pos->Update(&vertices[0], 0, vertices.size() * sizeof(glm::vec2));

			std::vector<glm::u8vec4> colors;
			colors.emplace_back(255,0,0,255);
			colors.emplace_back(0,255,0,255);
			colors.emplace_back(0,0,255,255);
			colors.emplace_back(255,0,0,255);			
			col->Update(&colors[0], 0, colors.size() * sizeof(glm::u8vec4));			

			glm::u8vec4 x;

			std::vector<uint8_t> indices;
			indices.emplace_back(0);
			indices.emplace_back(1);
			indices.emplace_back(2);
			indices.emplace_back(2);
			indices.emplace_back(1);
			indices.emplace_back(3);
			ab->UpdateIndicies(indices);
			*/

			//setColor(255,255,255);
			setOrder(0);
		}
		virtual ~SquareRenderable() {}
	private:
		SquareRenderable(const SquareRenderable&);
		SquareRenderable& operator=(const SquareRenderable&);
	};
	typedef std::shared_ptr<SquareRenderable> SquareRenderablePtr;
}

class SimpleRenderable : public KRE::SceneObject
{
public:
	explicit SimpleRenderable(const rect& r, const KRE::Color& color)
		: KRE::SceneObject("simple_renderable")
	{
		init();

		setColor(color);

		const float vx1 = static_cast<float>(r.x1());
		const float vy1 = static_cast<float>(r.y1());
		const float vx2 = static_cast<float>(r.x2());
		const float vy2 = static_cast<float>(r.y2());

		std::vector<glm::vec2> vc;
		vc.emplace_back(vx1, vy2);
		vc.emplace_back(vx1, vy1);
		vc.emplace_back(vx2, vy1);

		vc.emplace_back(vx2, vy1);
		vc.emplace_back(vx2, vy2);
		vc.emplace_back(vx1, vy2);
		attribs_->update(&vc);
	}
	void init(KRE::DrawMode draw_mode = KRE::DrawMode::TRIANGLES)
	{
		using namespace KRE;
		setShader(ShaderProgram::getProgram("simple"));

		auto as = DisplayDevice::createAttributeSet();
		attribs_.reset(new KRE::Attribute<glm::vec2>(AccessFreqHint::DYNAMIC, AccessTypeHint::DRAW));
		attribs_->addAttributeDesc(AttributeDesc(AttrType::POSITION, 2, AttrFormat::FLOAT, false));
		as->addAttribute(AttributeBasePtr(attribs_));
		as->setDrawMode(draw_mode);
		
		addAttributeSet(as);
	}
private:
	std::shared_ptr<KRE::Attribute<glm::vec2>> attribs_;
};


struct SimpleTextureHolder : public KRE::Blittable
{
	SimpleTextureHolder(const std::string& filename) {
		using namespace KRE;
		setColor(1.0f, 1.0f, 1.0f, 1.0f);
		auto tex = Texture::createTexture(filename, TextureType::TEXTURE_2D, 4);
		tex->setFiltering(-1, Texture::Filtering::LINEAR, Texture::Filtering::LINEAR, Texture::Filtering::POINT);
		tex->setAddressModes(-1, Texture::AddressMode::BORDER, Texture::AddressMode::BORDER);
		setTexture(tex);
	}
};

KRE::ShaderProgramPtr generateBlurShader(const std::string& name, float sigma, int radius)
{
	using namespace KRE;

	std::vector<float> std_gaussian_weights;
	float weights_sum = 0;
	const float sigma_pow_2 = std::pow(sigma, 2.0f);
	const float term1 = (1.0f / std::sqrt(2.0f * static_cast<float>(M_PI) * sigma_pow_2));
	for(int n = 0; n < radius + 2; ++n) {
		std_gaussian_weights.emplace_back(term1 * std::exp(-std::pow(static_cast<float>(n), 2.0f) / (2.0f * sigma_pow_2)));
		weights_sum += (n == 0 ? 1.0f : 2.0f) * std_gaussian_weights.back();
	}
	// normalise weights
	for(auto& weight : std_gaussian_weights) {
		weight /= weights_sum;
	}

	int optimized_offsets = std::min(radius/2 + (radius%2), 7);
	std::vector<float> opt_gaussian_weights;
	opt_gaussian_weights.resize(optimized_offsets);
	for(int n = 0; n < optimized_offsets; ++n) {
		const float first_weight = std_gaussian_weights[n * 2 + 1];
		const float second_weight = std_gaussian_weights[n * 2 + 2];
		const float sum_weights = first_weight + second_weight;
		opt_gaussian_weights[n] = (first_weight * static_cast<float>(n * 2 + 1) + second_weight * static_cast<float>(n * 2 + 1)) / sum_weights;
	}

	std::stringstream v_shader;
	v_shader << "uniform mat4 mvp_matrix;\n"
			<< "attribute vec2 position;\n"
			<< "attribute vec4 texcoord;\n"
			<< "uniform float texel_width_offset;\n"
			<< "uniform float texel_height_offset;\n"
			<< "varying vec2 blur_coords[" << (optimized_offsets * 2 + 1) << "];\n"
			<< "void main() {\n"
			<< "    gl_Position = mvp_matrix * vec4(position, 0.0, 1.0);\n"
			<< "    vec2 step_offset = vec2(texel_width_offset, texel_height_offset);\n"
			<< "    blur_coords[0] = texcoord.xy;\n"
			;
	for(int n = 0; n < optimized_offsets; ++n) {
		v_shader << "    blur_coords[" << (n * 2 + 1) << "] = texcoord.xy + step_offset * " << opt_gaussian_weights[n] << ";\n";
		v_shader << "    blur_coords[" << (n * 2 + 2) << "] = texcoord.xy + step_offset * " << opt_gaussian_weights[n] << ";\n";
	}
	v_shader << "}\n";

	std::stringstream f_shader;
	f_shader << "uniform sampler2D tex_map;\n"
		<< "varying highp vec2 blur_coords[" << (optimized_offsets * 2 + 1) << "];\n"
		<< "void main() {\n"
		<< "    lowp vec4 sum = vec4(0.0);\n"
		<< "    sum += texture2D(tex_map, blur_coords[0]) * " << std_gaussian_weights[0] << ";\n"
		;
	for(int n = 0; n < optimized_offsets; ++n) {
		const float first_weight = std_gaussian_weights[n * 2 + 1];
		const float second_weight = std_gaussian_weights[n * 2 + 2];
		const float sum_weights = first_weight + second_weight;
		f_shader 
			<< "    sum += texture2D(tex_map, blur_coords[" << (n * 2 + 1) << "]) * " << sum_weights << ";\n"
			<< "    sum += texture2D(tex_map, blur_coords[" << (n * 2 + 2) << "]) * " << sum_weights << ";\n"
			;
	}
	f_shader << "gl_FragColor = sum;\n}\n";

	variant_builder sp;
	sp.add("vertex", v_shader.str());
	sp.add("fragment", f_shader.str());
	sp.add("name", name);
	ShaderProgram::loadFromVariant(sp.build());
	return ShaderProgram::getProgram(name);
}

struct BlurredBlittable : public KRE::Blittable
{
	explicit BlurredBlittable(const std::string& filename) {
		using namespace KRE;
		setColor(1.0f, 1.0f, 1.0f, 1.0f);
		//auto shader = ShaderProgram::getProgram("blur1");
		//auto shader = generateBlurShader("blur-5.0-4", 5.0f, 9);
		auto shader = ShaderProgram::getProgram("blur2");
		two_ = shader->getUniform("texel_width_offset");
		tho_ = shader->getUniform("texel_height_offset");

		setShader(shader);

		auto tex = Texture::createTexture(filename, TextureType::TEXTURE_2D, 4);
		tex->setFiltering(-1, Texture::Filtering::LINEAR, Texture::Filtering::LINEAR, Texture::Filtering::POINT);
		tex->setAddressModes(-1, Texture::AddressMode::CLAMP, Texture::AddressMode::CLAMP);
		setTexture(tex);

		shader->setUniformValue(two_, 1.0f / (tex->width()-1));
		shader->setUniformValue(tho_, /*1.0f / (tex->height()-1)*/0.0f);
	}
	int two_;
	int tho_;
};

struct FreeTextureHolder : public KRE::SceneObject
{
	FreeTextureHolder(KRE::SurfacePtr surface)
		: KRE::SceneObject("FreeTextureHolder") 
	{
		using namespace KRE;
		auto tex = Texture::createTexture(surface);
		//tex->setFiltering(Texture::Filtering::LINEAR, Texture::Filtering::LINEAR, Texture::Filtering::POINT);
		//tex->setAddressModes(Texture::AddressMode::BORDER, Texture::AddressMode::BORDER);
		setTexture(tex);
		init();
	}
	FreeTextureHolder(KRE::TexturePtr tex)
		: KRE::SceneObject("FreeTextureHolder") 
	{
		using namespace KRE;
		setTexture(tex);
		init();
	}
	FreeTextureHolder(const std::string& filename)
		: KRE::SceneObject("FreeTextureHolder") 
	{
		using namespace KRE;
		auto tex = Texture::createTexture(filename, TextureType::TEXTURE_2D, 4);
		//tex->setFiltering(Texture::Filtering::LINEAR, Texture::Filtering::LINEAR, Texture::Filtering::POINT);
		tex->setAddressModes(-1, Texture::AddressMode::BORDER, Texture::AddressMode::BORDER);
		setTexture(tex);
		init();
	}
	void init()
	{
		setDrawRect(rect(0, 0, getTexture()->surfaceWidth(), getTexture()->surfaceHeight()));
		using namespace KRE;
		setColor(1.0f, 1.0f, 1.0f, 1.0f);
		auto as = DisplayDevice::createAttributeSet();
		attribs_.reset(new Attribute<vertex_texcoord>(AccessFreqHint::DYNAMIC, AccessTypeHint::DRAW));
		attribs_->addAttributeDesc(AttributeDesc(AttrType::POSITION, 2, AttrFormat::FLOAT, false, sizeof(vertex_texcoord), offsetof(vertex_texcoord, vtx)));
		attribs_->addAttributeDesc(AttributeDesc(AttrType::TEXTURE,  2, AttrFormat::FLOAT, false, sizeof(vertex_texcoord), offsetof(vertex_texcoord, tc)));
		as->addAttribute(AttributeBasePtr(attribs_));
		as->setDrawMode(DrawMode::TRIANGLE_STRIP);
		
		addAttributeSet(as);
	}
	void preRender(const KRE::WindowPtr& wm) override
	{
		const float offs_x = 0.0f;
		const float offs_y = 0.0f;
		// XXX we should only do this if things changed.
		const float vx1 = draw_rect_.x() + offs_x;
		const float vy1 = draw_rect_.y() + offs_y;
		const float vx2 = draw_rect_.x2() + offs_x;
		const float vy2 = draw_rect_.y2() + offs_y;

		const rectf& r = getTexture()->getSourceRectNormalised();

		std::vector<KRE::vertex_texcoord> vertices;
		vertices.emplace_back(glm::vec2(vx1,vy1), glm::vec2(r.x(),r.y()));
		vertices.emplace_back(glm::vec2(vx2,vy1), glm::vec2(r.x2(),r.y()));
		vertices.emplace_back(glm::vec2(vx1,vy2), glm::vec2(r.x(),r.y2()));
		vertices.emplace_back(glm::vec2(vx2,vy2), glm::vec2(r.x2(),r.y2()));
		getAttributeSet().back()->setCount(vertices.size());
		attribs_->update(&vertices);
	}
	template<typename T>
	void setDrawRect(const geometry::Rect<T>& r) {
		draw_rect_ = r.template as_type<float>();
	}
private:
	std::shared_ptr<KRE::Attribute<KRE::vertex_texcoord>> attribs_;
	rectf draw_rect_;
};

void set_alpha_masks()
{
	using namespace KRE;
	std::vector<Color> alpha_colors;

	auto surf = Surface::create("alpha-colors.png");
	surf->iterateOverSurface([&alpha_colors](int x, int y, int r, int g, int b, int a) {
		alpha_colors.emplace_back(r, g, b);
		LOG_INFO("Added alpha color: (" << r << "," << g << "," << b << ")");	
	});

	Surface::setAlphaFilter([=](int r, int g, int b) {
		for(auto& c : alpha_colors) {
			if(c.ri() == r && c.gi() == g && c.bi() == b) {
				return true;
			}
		}
		return false;
	});
}

struct water_distort_uniforms
{
	unsigned int texture;
	glm::mat4 mvp;
	float cycle;
	glm::vec4 sprite_area;
	glm::vec4 draw_area;
	float intensity;
	glm::vec4 water_area[2];
};


void texture_packing_test()
{
	using namespace KRE;
	const std::string tile_file1 = "brown-rock1.png";
	const std::string tile_file2 = "brown-rock1.png";
	SurfacePtr s1 = Surface::create(tile_file1);
	SurfacePtr s2 = Surface::create(tile_file2);
	std::vector<SurfaceAreas> sa;
	sa.emplace_back(s1);
	sa.back().addRect(16, 0, 16, 32);
	sa.back().addRect(32, 0, 16, 32);
	sa.emplace_back(s2);
	sa.back().addRect(0, 0, 16, 32);
	sa.back().addRect(16, 0, 16, 32);
	sa.back().addRect(0, 32, 16, 32);
	sa.back().addRect(16, 32, 16, 32);
	sa.back().addRect(32, 32, 16, 32);
	sa.back().addRect(48, 32, 16, 32);

	Packer tp(sa, 2048, 2048);
	for(auto& r : tp) {
		LOG_DEBUG("New rect: " << r);
	}
	// output surface available as tp.getOutputSurface()
}

std::vector<float> generate_gaussian(float sigma, int radius = 4)
{
	std::vector<float> std_gaussian_weights;
	float weights_sum = 0;
	const float sigma_pow_2 = std::pow(sigma, 2.0f);
	const float term1 = (1.0f / std::sqrt(2.0f * static_cast<float>(M_PI) * sigma_pow_2));
	for(int n = 0; n < radius + 1; ++n) {
		std_gaussian_weights.emplace_back(term1 * std::exp(-std::pow(static_cast<float>(n), 2.0f) / (2.0f * sigma_pow_2)));
		weights_sum += (n == 0 ? 1.0f : 2.0f) * std_gaussian_weights.back();
	}
	// normalise weights
	for(auto& weight : std_gaussian_weights) {
		weight /= weights_sum;
	}

	std::vector<float> res;
	for(auto it = std_gaussian_weights.crbegin(); it != std_gaussian_weights.crend(); ++it) {
		res.emplace_back(*it);
	}
	for(auto it = std_gaussian_weights.cbegin()+1; it != std_gaussian_weights.cend(); ++it) {
		res.emplace_back(*it);
	}

	std::stringstream ss;
	ss << "Gaussian(sigma=" << sigma << ", radius=" << radius << "):";
	for(auto it = std_gaussian_weights.crbegin(); it != std_gaussian_weights.crend(); ++it) {
		ss << " " << (*it);
	}
	//ss << " " << std_gaussian_weights[0];
	for(auto it = std_gaussian_weights.cbegin()+1; it != std_gaussian_weights.cend(); ++it) {
		ss << " " << (*it);
	}
	LOG_DEBUG(ss.str());
	return res;
}

point hex_to_pixel(const point& p) 
{
	const int tile_size = 72;
	return point((tile_size * 3 * p.x) / 2, static_cast<int>(tile_size * 1.7320508075688772935274463415059 * (p.y + p.x/2)));
}

class CastleDef
{
public:
	enum Direction { BOTTOM_LEFT, BOTTOM_RIGHT, LEFT, RIGHT, TOP_LEFT, TOP_RIGHT };

	struct CastlePart {
		rect r_;
		point offs_;
		std::array<int, 4> border_;
	};

	explicit CastleDef(const std::string& name, const variant& node) 
		: name_(name)
	{
		using namespace KRE;

		ASSERT_LOG(node.has_key("image"), "No 'image' attribute in castle definition.");
		const std::string tex_name = node["image"].as_string();		
		texture_ = Texture::createTexture(tex_name, TextureType::TEXTURE_2D, 4);
		//texture_->setFiltering(Texture::Filtering::LINEAR, Texture::Filtering::LINEAR, Texture::Filtering::POINT);
		texture_->setAddressModes(-1, Texture::AddressMode::BORDER, Texture::AddressMode::BORDER);

		ASSERT_LOG(node.has_key("concave") && node.has_key("convex"),
			"Castle definition must have 'concave' and 'convex' attributes.");

		concave_ = parse_dir(node["concave"]);
		convex_ = parse_dir(node["convex"]);
	}

	KRE::TexturePtr getTexture() const { return texture_; }
	const CastlePart& getConcaveDef(Direction dir) const { return concave_[dir]; }
	const CastlePart& getConvexDef(Direction dir) const { return convex_[dir]; }
private:
	CastleDef() = delete;
	const CastleDef& operator=(CastleDef const&) = delete;
	CastleDef(CastleDef const&) = delete;

	std::vector<CastlePart> parse_dir(const variant& node)
	{
		std::vector<CastlePart> res;
		const std::vector<std::string> keys = { "bl", "br", "l", "r", "tl", "tr" };
		ASSERT_LOG(node.is_map(), "must be a map type.");
		for(auto key : keys) {
			ASSERT_LOG(node.has_key(key), "Must have attribute '" << key << "' in definition.");
			const variant& dir = node[key];
			ASSERT_LOG(dir.has_key("rect"), "Attribute '" << key << "' must have 'rect' definition.");
			CastlePart cp;
			cp.r_ = rect(dir["rect"]);
			if(dir.has_key("border")) {
				ASSERT_LOG(dir["border"].num_elements() == 4, "'border' attribute must be list of 4 integers.");
				for(int n = 0; n != 4; ++n) {
					cp.border_[n] = dir["border"][n].as_int32();
				}
			} else {
				for(int n = 0; n != 4; ++n) {
					cp.border_[n] = 0;
				}
			}
			if(dir.has_key("offset")) {
				cp.offs_ = point(dir["offset"]);
			} else {
				cp.offs_.x = cp.offs_.y = 0;
			}
			res.emplace_back(cp);
		}
		return res;
	}

	std::string name_;
	KRE::TexturePtr texture_;
	std::vector<CastlePart> convex_;
	std::vector<CastlePart> concave_;
};

typedef std::shared_ptr<CastleDef> CastleDefPtr;

class Castle;
typedef std::shared_ptr<Castle> CastlePtr;

class Castle : public KRE::SceneObject
{
	struct cons_access {
		explicit cons_access(int) {}
	};
public:
	explicit Castle(const cons_access&, const std::string& name, const CastleDefPtr& def, const std::set<point>& locs) 
		: KRE::SceneObject("Castle"),
		  name_(name)
	{
		using namespace KRE;

		setTexture(def->getTexture());
		init();

		std::vector<KRE::vertex_texcoord> verts;
		// Process locations, generate a list of corners that need to be placed.
		// N, NE, SE, S, SW, NW
		enum Directions { N, NE, SE, S, SW, NW };
		std::vector<point> dir_list = { point(0,-1), point(1,-1), point(1,1), point(0,1), point(-1,0), point(-1,-1) };
		for(auto& loc : locs) {
			auto it_n = locs.find(loc + dir_list[N]);
			auto it_nw = locs.find(loc + dir_list[NE]);
			auto it_ne = locs.find(loc + dir_list[NW]);
			auto it_s = locs.find(loc + dir_list[S]);
			auto it_sw = locs.find(loc + dir_list[SE]);
			auto it_se = locs.find(loc + dir_list[SW]);

			if(it_n == locs.end() && it_ne == locs.end()) {
				// place TR convex
				addCorner(&verts, def->getConvexDef(CastleDef::Direction::TOP_RIGHT), loc, CastleDef::Direction::TOP_RIGHT);
			} else {
				if(!(it_n != locs.end() && it_ne != locs.end())) {
					// place TR concave
					addCorner(&verts, def->getConcaveDef(CastleDef::Direction::TOP_RIGHT), loc, CastleDef::Direction::TOP_RIGHT);
				}
			}

			if(it_n == locs.end() && it_nw == locs.end()) {
				// place TL convex
				addCorner(&verts, def->getConvexDef(CastleDef::Direction::TOP_LEFT), loc, CastleDef::Direction::TOP_LEFT);
			} else {
				if(!(it_n != locs.end() && it_nw != locs.end())) {
					// place TL concave
					addCorner(&verts, def->getConcaveDef(CastleDef::Direction::TOP_LEFT), loc, CastleDef::Direction::TOP_LEFT);
				}
			}
			
			if(it_nw == locs.end() && it_sw == locs.end()) {
				// place L convex
				addCorner(&verts, def->getConvexDef(CastleDef::Direction::LEFT), loc, CastleDef::Direction::LEFT);
			} else {
				if(it_nw == locs.end() || it_sw == locs.end()) {
					// place L concave
					addCorner(&verts, def->getConcaveDef(CastleDef::Direction::LEFT), loc, CastleDef::Direction::LEFT);
				}
			}

			if(it_ne == locs.end() && it_se == locs.end()) {
				// place R convex
				addCorner(&verts, def->getConvexDef(CastleDef::Direction::RIGHT), loc, CastleDef::Direction::RIGHT);
			} else {
				if(!(it_ne != locs.end() && it_se != locs.end())) {
					// place R concave
					addCorner(&verts, def->getConcaveDef(CastleDef::Direction::RIGHT), loc, CastleDef::Direction::RIGHT);
				}
			}

			if(it_s == locs.end() && it_se == locs.end()) {
				// place BR convex
				addCorner(&verts, def->getConvexDef(CastleDef::Direction::BOTTOM_RIGHT), loc, CastleDef::Direction::BOTTOM_RIGHT);
			} else {
				if(!(it_s != locs.end() && it_se != locs.end())) {
					// place BR concave
					addCorner(&verts, def->getConcaveDef(CastleDef::Direction::BOTTOM_RIGHT), loc, CastleDef::Direction::BOTTOM_RIGHT);
				}
			}

			if(it_s == locs.end() && it_sw == locs.end()) {
				// place BL convex
				addCorner(&verts, def->getConvexDef(CastleDef::Direction::BOTTOM_LEFT), loc, CastleDef::Direction::BOTTOM_LEFT);
			} else {
				if(!(it_s != locs.end() && it_sw != locs.end())) {
					// place BL concave
					addCorner(&verts, def->getConcaveDef(CastleDef::Direction::BOTTOM_LEFT), loc, CastleDef::Direction::BOTTOM_LEFT);
				}
			}
		}

		attribs_->update(&verts);
	}

	static CastlePtr create(const std::string& name, const CastleDefPtr& def, const std::set<point>& locs)
	{
		return std::make_shared<Castle>(cons_access{0}, name, def, locs);
	}
private:
	void init()
	{
		using namespace KRE;
		setColor(1.0f, 1.0f, 1.0f, 1.0f);
		auto as = DisplayDevice::createAttributeSet();
		attribs_.reset(new Attribute<vertex_texcoord>(AccessFreqHint::DYNAMIC, AccessTypeHint::DRAW));
		attribs_->addAttributeDesc(AttributeDesc(AttrType::POSITION, 2, AttrFormat::FLOAT, false, sizeof(vertex_texcoord), offsetof(vertex_texcoord, vtx)));
		attribs_->addAttributeDesc(AttributeDesc(AttrType::TEXTURE,  2, AttrFormat::FLOAT, false, sizeof(vertex_texcoord), offsetof(vertex_texcoord, tc)));
		as->addAttribute(AttributeBasePtr(attribs_));
		as->setDrawMode(DrawMode::TRIANGLES);
		
		addAttributeSet(as);
	}
	
	void addCorner(std::vector<KRE::vertex_texcoord>* verts, const CastleDef::CastlePart& def, const point& loc, CastleDef::Direction dir)
	{
		// loc is in hex co-ords, need to convert to pixel co-ords.
		point p = hex_to_pixel(loc);
		// offsets to the bl, br, l, r, tl, tr based on a tile size of 72.
		std::vector<point> offset = { point(18,71), point(53, 71), point(0, 35), point(71, 35), point(18, 0), point(53, 0) };
		const float vx1 = static_cast<float>(p.x + offset[dir].x + def.offs_.x - def.border_[0] - (def.r_.w()/2));
		const float vy1 = static_cast<float>(p.y + offset[dir].y + def.offs_.y - def.border_[1] - (def.r_.h()/2));
		const float vx2 = vx1 + def.r_.w();
		const float vy2 = vy1 + def.r_.h();

		const rectf& r = getTexture()->getTextureCoords(0, def.r_);

		verts->emplace_back(glm::vec2(vx1,vy1), glm::vec2(r.x(),r.y()));
		verts->emplace_back(glm::vec2(vx2,vy1), glm::vec2(r.x2(),r.y()));
		verts->emplace_back(glm::vec2(vx1,vy2), glm::vec2(r.x(),r.y2()));

		verts->emplace_back(glm::vec2(vx1,vy2), glm::vec2(r.x(),r.y2()));
		verts->emplace_back(glm::vec2(vx2,vy1), glm::vec2(r.x2(),r.y()));
		verts->emplace_back(glm::vec2(vx2,vy2), glm::vec2(r.x2(),r.y2()));
	}

	Castle() = delete;
	const Castle& operator=(Castle const&) = delete;
	Castle(Castle const&) = delete;

	std::string name_;
	std::shared_ptr<KRE::Attribute<KRE::vertex_texcoord>> attribs_;
};

namespace 
{
	typedef std::map<std::string, CastleDefPtr> castle_def_map;
	castle_def_map& get_castle_def() 
	{
		static castle_def_map res;
		return res;
	}
}

void load_castle_definitions(const variant& castle_def)
{
	ASSERT_LOG(castle_def.is_map(), "Castle definitions must be a map.");
	for(const auto& def : castle_def.as_map()) {
		const std::string name = def.first.as_string();
		get_castle_def()[name] = std::make_shared<CastleDef>(name, def.second);
	}
}

int main(int argc, char *argv[])
{
#ifdef _MSC_VER
	SetProcessDPIAware();
#endif

	std::list<double> smoothed_time;
	double cumulative_time = 0.0;
	int cnt = 0;

	using namespace KRE;

	SDL::SDL_ptr manager(new SDL::SDL());

	SDL_LogSetPriority(SDL_LOG_CATEGORY_APPLICATION, SDL_LOG_PRIORITY_DEBUG);

	WindowManager wm("SDL");

	variant_builder hints;
	hints.add("renderer", "opengl");
	hints.add("dpi_aware", true);
	hints.add("use_vsync", true);
	int neww = 1600, newh = 900;
	//if(!autoWindowSize(neww, newh)) {
	//	LOG_DEBUG("Couldn't get automatic window size. Defaulting to " << neww << "x" << newh);
	//}
	LOG_DEBUG("Creating window of size: " << neww << "x" << newh);
	auto main_wnd = wm.createWindow(neww, newh, hints.build());
	main_wnd->enableVsync(true);
	const float aspect_ratio = static_cast<float>(neww) / newh;

	std::map<std::string, std::string> font_paths;
#if defined(__linux__)
	LOG_DEBUG("setting image file filter to 'images/'");
	Surface::setFileFilter(FileFilterType::LOAD, [](const std::string& fname) { return "images/" + fname; });
	Surface::setFileFilter(FileFilterType::SAVE, [](const std::string& fname) { return "images/" + fname; });
	
	font_paths["FreeSans.ttf"] = "data/fonts/FreeSans.ttf";
#else
	LOG_DEBUG("setting image file filter to '../images/'");
	Surface::setFileFilter(FileFilterType::LOAD, [](const std::string& fname) { return "../images/" + fname; });
	Surface::setFileFilter(FileFilterType::SAVE, [](const std::string& fname) { return "../images/" + fname; });

	font_paths["FreeSans.ttf"] = "../data/fonts/FreeSans.ttf";
#endif
	Font::setAvailableFonts(font_paths);

	/*auto surf = Surface::create("test_image.png");
	for(auto col : *surf) {
		LOG_DEBUG(col.red << "," << col.green << "," << col.blue << "," << col.alpha);
	}*/
	
	set_alpha_masks();	
	
	// XXX should a scenegraph be created from a specific window? It'd solve a couple of issues
	SceneGraphPtr scene = SceneGraph::create("main");
	SceneNodePtr root = scene->getRootNode();
	root->setNodeName("root_node");
	/*auto scenecam = std::make_shared<Camera>("cam0", 0, neww, 0, newh);
	scenecam->attachFrustum(std::make_shared<Frustum>());
	root->attachCamera(scenecam);
	auto sunlight = std::make_shared<Light>("the_sun", glm::vec3(1.0f, 1.0f, 1.0f));
	sunlight->setAmbientColor(Color(1.0f,1.0f,1.0f,1.0f));
	root->attachLight(0, sunlight);*/

	DisplayDevice::getCurrent()->setDefaultCamera(std::make_shared<Camera>("ortho1", 0, neww, 0, newh));

	auto rman = std::make_shared<RenderManager>();
	auto rq = rman->addQueue(0, "opaques");


#if defined(__linux__)
	std::string shader_test_file = "data/shaders.cfg";
#else
	std::string shader_test_file = "../data/shaders.cfg";
#endif
	ShaderProgram::loadFromVariant(json::parse_from_file(shader_test_file));

	std::shared_ptr<FreeTextureHolder> base_tile = std::make_shared<FreeTextureHolder>("cobbles-keep.png");
	base_tile->setPosition(neww/4, newh/4);
	base_tile->setScale(glm::vec3(2.0f, 2.0f, 1.0f));
	root->attachObject(base_tile);

#if defined(__linux__)
	std::string castle_test_file = "data/castles.json";
#else
	std::string castle_test_file = "../data/castles.json";
#endif

	variant castle_def;
	try {
		castle_def = json::parse_from_file(castle_test_file);
	} catch(json::parse_error& e) {
		ASSERT_LOG(false, "Error parsing file '" << castle_test_file << "', error: " << e.what());
	}

	load_castle_definitions(castle_def);

	std::set<point> castle_hexes;
	castle_hexes.emplace(0, 1);
	//castle_hexes.emplace(0, 1);
	auto castle1 = Castle::create("test", get_castle_def()["human_castle"], castle_hexes);
	castle1->setPosition(neww/4, newh/4);
	castle1->setScale(glm::vec3(2.0f, 2.0f, 1.0f));
	castle1->setOrder(10);
	root->attachObject(castle1);

	Uint32 last_tick_time = SDL_GetTicks();

	SDL_Event e;
	bool done = false;
	profile::timer timer;
	while(!done) {
		timer.start();
		while(SDL_PollEvent(&e)) {
			if(e.type == SDL_KEYUP && e.key.keysym.scancode == SDL_SCANCODE_ESCAPE) {
				done = true;
			} else if(e.type == SDL_KEYDOWN) {
				LOG_DEBUG("KEY PRESSED: " << SDL_GetKeyName(e.key.keysym.sym) << " : " << e.key.keysym.sym << " : " << e.key.keysym.scancode);
			} else if(e.type == SDL_QUIT) {
				done = true;
			}
		}

		main_wnd->setClearColor(KRE::Color(0, 0, 0, 255));
		main_wnd->clear(ClearFlags::ALL);

		// Called once a cycle before rendering.
		Uint32 current_tick_time = SDL_GetTicks();
		scene->process((current_tick_time - last_tick_time) / 1000.0f);
		last_tick_time = current_tick_time;

		scene->renderScene(rman);
		rman->render(main_wnd);

		double t1 = timer.check();
		if(t1 < 1.0/50.0) {
		//	SDL_Delay(Uint32((1.0/50.0-t1)*1000.0));
		}
		double t = timer.check();

		smoothed_time.push_back(t);
		cumulative_time += t;
		if(++cnt > 10) {
			cnt = 0;
			//LOG_DEBUG("FPS: " << (smoothed_time.size()/cumulative_time) << ", Time: " << t1*1000.0);
			LOG_DEBUG("Draw Time: " << (t * 1000000.0) << " us.");
		}
		if(smoothed_time.size() > 50) {
			cumulative_time -= smoothed_time.front();
			smoothed_time.pop_front();
		}

		main_wnd->swap();
	}
	return 0;
}
