pico-8 cartridge // http://www.pico-8.com
version 29
__lua__

------------------------------------
-- Base Objects
------------------------------------

-- creates a new object by calling obj = object:extend()
object={}
function object:extend(kob)
  kob=kob or {}
  kob.extends=self
  return setmetatable(kob,{
   __index=self,
   __call=function(self,ob)
	   ob=setmetatable(ob or {},{__index=kob})
	   local ko,init_fn=kob
	   while ko do
	    if ko.init and ko.init~=init_fn then
	     init_fn=ko.init
	     init_fn(ob)
	    end
	    ko=ko.extends
	   end
	   return ob
  	end
  })
end

vector={}
vector.__index=vector
 -- operators: +, -, *, /
 function vector:__add(b)
  return v(self.x+b.x,self.y+b.y)
 end
 function vector:__sub(b)
  return v(self.x-b.x,self.y-b.y)
 end
 function vector:__mul(m)
  return v(self.x*m,self.y*m)
 end
 function vector:__div(d)
  return v(self.x/d,self.y/d)
 end
 function vector:__unm()
  return v(-self.x,-self.y)
 end
function vector:__neq(v)
  return not (self.x==v.x and self.y==v.y)
end
function vector:__eq(v)
  return self.x==v.x and self.y==v.y
end
 -- dot product
 function vector:dot(v2)
  return self.x*v2.x+self.y*v2.y
 end
 -- normalization
 function vector:norm()
  return self/sqrt(#self)
 end
 -- length
 function vector:len()
  return sqrt(#self)
 end
 -- the # operator returns
 -- length squared since
 -- that's easier to calculate
 function vector:__len()
  return self.x^2+self.y^2
 end
 -- printable string
 function vector:str()
  return self.x..","..self.y
 end

-- creates a new vector with
-- the x,y coords specified
function v(x,y)
 return setmetatable({
  x=x,y=y
 },vector)
end



entity=object:extend(
  {
    t=0,
    spawns={}
  }
)

 -- common initialization
 -- for all entity types
function entity:init()  
  if self.sprite then
   self.sprite=deep_copy(self.sprite)
   if not self.render then
    self.render=spr_render
   end
  end
end
 -- called to transition to
 -- a new state - has no effect
 -- if the entity was in that
 -- state already
function entity:become(state)
  if state~=self.state then
   self.state,self.t=state,0
  end
end
-- checks if entity has 'tag'
-- on its list of tags
function entity:is_a(tag)
  if (not self.tags) return false
  for i=1,#self.tags do
   if (self.tags[i]==tag) return true
  end
  return false
end
 -- called when declaring an
 -- entity class to make it
 -- spawn whenever a tile
 -- with a given number is
 -- encountered on the level map
function entity:spawns_from(...)
  for tile in all({...}) do
   entity.spawns[tile]=self
  end
end

------------------------------------
-- dynamic objects
------------------------------------

dynamic=entity:extend({
    maxvel=1,
    acc=0.5,
    fric=0.5,
    vel=v(0,0),
    dir=v(0,0)
  })

------------------------------------
-- collision boxes
------------------------------------

-- collision boxes are just
-- axis-aligned rectangles
cbox=object:extend()
 -- moves the box by the
 -- vector v and returns
 -- the result
 function cbox:translate(v)
  return cbox({
   xl=self.xl+v.x,
   yt=self.yt+v.y,
   xr=self.xr+v.x,
   yb=self.yb+v.y
  })
 end

 -- checks if two boxes
 -- overlap
 function cbox:overlaps(b)
  return
   self.xr>b.xl and
   b.xr>self.xl and
   self.yb>b.yt and
   b.yb>self.yt
 end

 -- calculates a vector that
 -- neatly separates this box
 -- from another. optionally
 -- takes a table of allowed
 -- directions
function cbox:sepv(b,allowed)
  local candidates={
    v(b.xl-self.xr,0),
    v(b.xr-self.xl,0),
    v(0,b.yt-self.yb),
    v(0,b.yb-self.yt)
  }
  if type(allowed)~="table" then
   allowed={true,true,true,true}
  end
  local ml,mv=32767
  for d,v in pairs(candidates) do
   if allowed[d] and #v<ml then
    ml,mv=#v,v
   end
  end

  return mv
end
 
 -- printable representation
 function cbox:str()
  return self.xl..","..self.yt..":"..self.xr..","..self.yb
 end

-- makes a new box
function box(xl,yt,xr,yb) 
 return cbox({
  xl=min(xl,xr),xr=max(xl,xr),
  yt=min(yt,yb),yb=max(yt,yb)
 })
end


------------------------------------
-- particles
--    common class for all
--    particles
-- Implements init(), update() and
-- render()
------------------------------------

particle=object:extend(
  {
    t=0,vel=v(0,0),
    lifetime=30
  }
)

------------------------------------
-- Bucket / Collisions
------------------------------------

c_bucket = {}

function do_movement()
  for e in all(entities) do
      if e.vel then
        for i=1,2 do
          e.pos.x+=e.vel.x/2
          collide_tile(e)
          
          e.pos.y+=e.vel.y/2
          collide_tile(e)
        end
      end
    end
end

function bkt_pos(e)
  local x,y=e.pos.x,e.pos.y
  return flr(shr(x,4)),flr(shr(y,4))
end

-- add entity to all the indexes
-- it belongs in the bucket
function bkt_insert(e)
  local x,y=bkt_pos(e)
  for t in all(e.tags) do
    local b=bkt_get(t,x,y)
    add(b,e)
  end

  e.bkt=v(x,y)
end

function bkt_remove(e)
  local x,y=e.bkt.x,e.bkt.y
  for t in all(e.tags) do
    local b=bkt_get(t,x,y)
    del(b,e)
  end
end

function bkt_get(t,x,y)
  local ind=t..":"..x..","..y
  if not c_bucket[ind] then
    c_bucket[ind]={}
  end
  return c_bucket[ind]
end

function bkt_update()  
  for e in all(entities) do
    bkt_update_entity(e)
  end
end

function bkt_update_entity(e)
  if not e.pos or not e.tags then return end
  local bx,by=bkt_pos(e)
  if not e.bkt or e.bkt.x~=bx or e.bkt.x~=by then
    if not e.bkt then
      bkt_insert(e)
    else
      bkt_remove(e)
      bkt_insert(e)
    end
  end
end

-- iterator that goes over
-- all entities with tag "tag"
-- that can potentially collide
-- with "e" - uses the bucket
-- structure described earlier.
function c_potentials(e,tag)
 local cx,cy=bkt_pos(e)
 local bx,by=cx-2,cy-1
 local bkt,nbkt,bi={},0,1
 return function()
  -- ran out of current bucket,
  -- find next non-empty one
  while bi>nbkt do
   bx+=1
   if (bx>cx+1) bx,by=cx-1,by+1
   if (by>cy+1) return nil
   bkt=bkt_get(tag,bx,by)
   nbkt,bi=#bkt,1
  end
  -- return next entity in
  -- current bucket and
  -- increment index
  local e=bkt[bi]
  bi+=1
  return e
 end 
end

function do_collisions()    
  	for e in all(entities) do
      collide(e)
    end
end

function collide(e)
  if not e.collides_with then return end
  if not e.hitbox then return end

  local ec=c_get_entity(e)

  ---------------------
  -- entity collision
  ---------------------
  for tag in all(e.collides_with) do
    --local bc=bkt_get(tag,e.bkt.x,e.bkt.y)
    for o in  c_potentials(e,tag) do  --all(entities[tag]) do
      -- create an object that holds the entity
      -- and the hitbox in the right position
      local oc=c_get_entity(o)
      -- call collide function on the entity
      -- that e collided with
      if o~=e and ec.b:overlaps(oc.b) then
        if ec.e.collide then 
          local func,arg=ec.e:collide(oc.e)
          if func then
            func(ec,oc,arg)            
          end
        end
      end

    end
  end
end

------------------------------------
-- Tile Collision
------------------------------------

function collide_tile(e)  
  -- do not collide if it's not set to
  if (not e.c_tile) return

  local ec=c_get_entity(e)

  local pos=tile_flag_at(ec.b, 1)

  for p in all(pos) do
    local oc={}
    oc.b=box(p.x,p.y,p.x+8,p.y+8)

    -- only allow pushing to empty spaces
    local dirs={v(-1,0),v(1,0),v(0,-1),v(0,1)}
    local allowed={}
    for i=1,4 do
      local np=v(p.x/8,p.y/8)+dirs[i]
      allowed[i]= not is_solid(np.x,np.y) and not (np.x < 0 or np.x > 127 or np.y < 0 or np.y > 63)
    end

    c_push_out(oc, ec, allowed)
    if (ec.e.tcollide) ec.e:tcollide()    
  end
end

-- get entity with the right position
-- for cboxes
function c_get_entity(e)
  local ec={e=e,b=e.hitbox}
  if (ec.b) ec.b=ec.b:translate(e.pos)
  return ec
end

function tile_at(cel_x, cel_y)
	return mget(cel_x, cel_y)
end

function is_solid(cel_x,cel_y)
  return fget(mget(cel_x, cel_y),1)
end

function solid_at(x, y, w, h)
	return #tile_flag_at(box(x,y,x+w,y+h), 1)>0
end

function tile_flag_at(b, flag)
  local pos={}

	for i=flr(b.xl/8), ((ceil(b.xr)-1)/8) do
		for j=flr(b.yt/8), ((ceil(b.yb)-1)/8) do
			if(fget(tile_at(i, j), flag)) then
				add(pos,{x=i*8,y=j*8})
			end
		end
	end

  return pos
end

-- reaction function, used by
-- returning it from :collide().
-- cause the other object to
-- be pushed out so it no
-- longer collides.
function c_push_out(oc,ec,allowed_dirs)
 local sepv=ec.b:sepv(oc.b,allowed_dirs)
 if not sepv then return end
 ec.e.pos+=sepv
 if ec.e.vel then
  local vdot=ec.e.vel:dot(sepv)
  if vdot<0 then   
   if sepv.x~=0 then ec.e.vel.x=0 end
   if sepv.y~=0 then ec.e.vel.y=0 end
  end
 end
 ec.b=ec.b:translate(sepv)
 return sepv
 end
-- inverse of c_push_out - moves
-- the object with the :collide()
-- method out of the other object.
-- function c_move_out(oc,ec,allowed)
--  return c_push_out(ec,oc,allowed)
-- end

------------------------------------
-- Entity Handling
------------------------------------

entities = {}
particles = {}
r_entities = {}

function update_draw_order(e, ndr)
    e.draw_order = e.draw_order or 3
    del(r_entities[e.draw_order], e)

    e.draw_order = ndr or 3
    add(r_entities[e.draw_order], e)
end

function p_add(p)  
  add(particles, p)
end

function p_remove(p)
  del(particles, p)
end

function p_update()
  for p in all(particles) do
    if p.pos and p.vel then
      p.pos+=p.vel
    end
    if (p.update) p:update()

    if (p.t > p.lifetime or p.done)p_remove(p)
    p.t+=1
  end
end

-- adds entity to all entries
-- of the table indexed by it's tags
function e_add(e)
  add(entities, e)

  local dr=e.draw_order or 3
  if (not r_entities[dr]) r_entities[dr]={}
  add(r_entities[dr],e)
end

function e_remove(e)
  del(entities, e)
  for tag in all(e.tags) do        
    if e.bkt then
      del(bkt_get(tag, e.bkt.x, e.bkt.y), e)
    end
  end

  del(r_entities[e.draw_order or 3],e)

  if e.destroy then e:destroy() end
end

-- loop through all entities and
-- update them based on their state
function e_update_all()  
  for e in all(entities) do
    if (e[e.state])e[e.state](e)
    if (e.update)e:update()
    e.t+=1

    if e.done then
      e_remove(e)
    end
  end
end

function e_draw_all()
  for i=0,7 do
    for e in all(r_entities[i]) do
      if (e.render)e:render()
    end
  end
end

function p_draw_all()
  for p in all(particles) do
    p:render()
  end
end

function spr_render(e)
  spr(e.sprite, e.pos.x, e.pos.y)
end

------------------------------------
-- Utils
------------------------------------

function sign(val)
  return val<0 and -1 or (val > 0 and 1 or 0)
end

function frac(val)
  return val-flr(val)
end

function ceil(val)
  if (frac(val)>0) return flr(val+sign(val)*1)
  return val
end

function approach(val,target,step)
  step=abs(step)
  return val < target and min(val+step,target) or max(val-step,target)
end

function clamp(low, hi, val)
	return (val < low) and low or (val > hi and hi or val)
end

function choose(arg)
    return arg[flr(rnd(#arg)) + 1]
end

function e_is_any(e, op)
  for i in all(e.tags) do
    for o in all(op) do
      if e:is_a(o) then return true end
    end
  end

  return false
end

function deep_copy(obj)
 if (type(obj)~="table") return obj
 local cpy={}
 setmetatable(cpy,getmetatable(obj))
 for k,v in pairs(obj) do
  cpy[k]=deep_copy(v)
 end
 return cpy
end

function shallow_copy(obj)
  if (type(obj)~="table") return obj
 local cpy={} 
 for k,v in pairs(obj) do
  cpy[k]=v
 end
 return cpy
end

------------------------------------
-- Boilerplate Code
------------------------------------

state = nil
palt(0, true)

-- Destroys everything from current state
function reset_state()

end

function change_state(new_state)
    state = new_state

    -- Destroy everything
    reset_state()

    state.init()
end

function _init()
    change_state(teststate)
end

function _update()
    state.update()
end

function _draw()
    state.draw()
end

--------------------------------------------------------------------------------------------
-- ACTUAL GAME CODE
--------------------------------------------------------------------------------------------

screen_size = 64
poke(0x5f2c, 3)

------------------------------------
-- Test State
------------------------------------

teststate = {}

function teststate.init()
				p = player{pos=v(screen_size / 2, screen_size / 2)}
    e_add(p)
				e_add(spawner{player=p})    
end

function teststate.update()
    e_update_all()
    bkt_update()
    do_movement()
    do_collisions()
    p_update()
end

function teststate.draw()
    cls()

    e_draw_all()
    p_draw_all()

    handle_camera()
end

-------------------------------
-- camera functions
-------------------------------

shake=0
s_amount=1
function handle_camera()
  camera()

  if shake > 0 then 
    shake-=1 
    camera(rnd(s_amount)-s_amount/2, rnd(s_amount)-s_amount/2)
  end
end

-------------------------------
-- entity: explosion particle
-------------------------------

-------------------------------
-- smoke particle
-------------------------------

smoke=particle:extend(
  {
    vel=v(0,0),
    c=9,
    v=0.1
  }
)

function smoke:init()
  self.vel=v(rnd(0.5)-0.25,-(rnd(1)+0.5))
  if (not self.r) self.r=rnd(1)+0.5
end

function smoke:update()
  self.r-=self.v
  if (self.r<=0) self.done=true
end

function smoke:render()
  if (not self.pos) return  
  circfill(self.pos.x, self.pos.y, self.r, self.c)
end

function explode(x, y, w, h)
  for i=0,10 do
    p_add(smoke{pos=v(x+w/2+rnd(1)-0.5, 
                      y+h/2+rnd(1)-0.5),
                c=choose({7,8,9}),r=rnd(2)+1,v=rnd(0.2)+0.2})
  end
end

-------------------------------
-- entity: dust particle
-------------------------------

dust=particle:extend({ })

-- adapted from: https://www.lexaloffle.com/bbs/?pid=58210
function add_new_dust(x,y,dx,dy,l,s,g,p,f)
	p_add(dust{
	    fade=f,
	    pos=v(x,y),
	    dx=dx,
	    dy=dy,
	    lifetime=l,
	    rad=s,
	    p=p,
	    col=0, --set to color
	    grav=g
	})
end

function dust:update() 
 --move the particle based on
 --the speed
 self.pos.x+=self.dx
 self.pos.y+=self.dy
 --and gravity
 self.dy+=self.grav

 --reduce the radius
 --this is set to 90%, but
 --could be altered
 self.rad*=self.p

 --set the color
 if type(self.fade)=="table" then
  --assign color from fade
  --this code works out how
  --far through the lifespan
  --the particle is and then
  --selects the color from the
  --table
  self.col=self.fade[flr(#self.fade*(self.t/self.lifetime))+1]
 else
  --just use a fixed color
  self.col=self.fade            
 end
end

function dust:render()
 circfill(self.pos.x,self.pos.y,self.rad,self.col)
end

-------------------------------
-- entity: turnable
-------------------------------

turnable=dynamic:extend({
    a=0,
   	size=v(1,1)
})

function turnable:render() 
		renderrot(self.sprite, self.pos, self.a, 0)
end

function renderrot(sprite, pos, a, ignored)
  rspr(8*sprite,
  	flr(sprite/16),
  	pos.x, pos.y,
  	a/360, 1, ignored)
end

function rspr(sx,sy,x,y,a,w,ignored)
  local ca,sa=cos(a),sin(a)
  local srcx,srcy
  local ddx0,ddy0=ca,sa
  local mask=shl(0xfff8,(w-1))
  w*=4
  ca*=w-0.5
  sa*=w-0.5
  local dx0,dy0=sa-ca+w,-ca-sa+w
  w=2*w-1
  for ix=0,w do
      srcx,srcy=dx0,dy0
      for iy=0,w do
          if band(bor(srcx,srcy),mask)==0 then
              local c=sget(sx+srcx,sy+srcy)
              if (c ~= ignored)	pset(x+ix,y+iy,c)
          end
          srcx-=ddy0
          srcy+=ddx0
      end
      dx0+=ddx0
      dy0+=ddy0
  end
end

-------------------------------
-- entity: changeable
-------------------------------

changeable=turnable:extend({
			 sprchange={}
})

function changeable:render()
		renderrot(self.sprite, self.pos, self.a, 0)
		currchange=self.sprchange[flr(#self.sprchange*(
		self:getcurrchange()))]
		if currchange ~= nil then
			renderrot(currchange, self.pos, self.a, 0)
		end
end

function changeable:getcurrchange() 
			return 0
end

-------------------------------
-- entity: player
-------------------------------

player=changeable:extend({
				maxspeed=1,
				acc=.25,
				friction=.2,
				
				sprite=13,
				hitbox=box(2,2,6,6),
				
    sprchange={2, 14, 15},
    hitboxes={box(2,2,6,6),
    										box(2,2,6,6)},
    
    vel=v(0,0),
    t=0,
    
    draw_order=4,
    tags={"player"},
    
    level=3,
    healthperlevel=2,
    health=2
})

function player:init()
  self:become("walking")
end

-- This update function will always get called
function player:update()
  
end

-- This one is a state-specific update function
function player:walking()
  if not btn(5) then
  	self:setspeed()
  end
  	
  self:setangle()
  
  if btnp(4) then
  	self:shoot()
  end
end

function player:setspeed() 
	prev = v(self.vel.x, self.vel.y)
  if (btn(0)) self.vel.x -= self.acc
  if (btn(1)) self.vel.x += self.acc
  if (btn(2)) self.vel.y -= self.acc
  if (btn(3)) self.vel.y += self.acc
		
	norm=self.vel:norm()
 	len=min(self.vel:len(),
 	self.maxspeed) 
 
  if prev == self.vel then
  	len=approach(len, 0,
  		self.friction)
  end
 
 	self.vel=norm * len
end

function player:setangle()
	if (btn(0)) self.a = 180
  if (btn(1)) self.a = 0
  if (btn(2)) self.a = 270
  if (btn(3)) self.a = 90
  
  if (btn(0) and btn(2)) self.a = 225
  if (btn(0) and btn(3)) self.a = 135
  if (btn(1) and btn(2)) self.a = 315
  if (btn(1) and btn(3)) self.a = 45
end

function player:shoot()
  e_add(bullet{
    pos=v(self.pos.x, self.pos.y),
    vel=v(0,0),
    shtangle=self.a,
    origin=self})
end

function player:take_hit(dmg)
  self.health-=dmg%self.healthperlevel
    shake+=10
    
    self.level-=flr(dmg/self.healthperlevel)
    if self.health<=0 then
    		self.level-=1
    		self.health = self.healthperlevel
    end

    if(self.level <= 0) then
      self.done = true
      explode(
        self.pos.x, self.pos.y,
        self.hitbox.xr-self.hitbox.xl,
        self.hitbox.yb-self.hitbox.yt)
    end
end

function player:getcurrchange() 
		return self.level/#self.sprchange
end

-------------------------------
-- entity: spawner
-------------------------------

spawner=entity:extend({
				reloadtime=10,
    t=0,
    player=nil
})

function spawner:init()
		e_add(enemy{pos=v(0,0), speed=rnd(2), vel=v(1,0), player=p})
		e_add(friend{pos=v(56,0), speed=rnd(2), vel=v(-1,0), player=p})
end

-------------------------------
-- entity: npc
-------------------------------

npc=changeable:extend({
		speed=1,
				reloadtime=10,
				reloadthresh=10,
				
    hitbox=box(0,0,8,8),
    sprite=4,
    vel=v(0,0),
    t=0,
    lastshot=0,
    player=nil,
    
    sprchange={5,6,7},
    changetime=180,
    
    tags={"npc"},
    collides_with={"player"},
    health=2
})

function npc:init()
  self:become("walking")
  norm = self.vel:norm()
  self.vel = norm*self.speed
end

-- This update function will always get called
function npc:update()
		if self.pos.x < -8 or self.pos.x > 64 
		or self.pos.y < -8 or self.pos.y > 64 then
			-- todo: improve this (was only for testing)
			self.vel *= -1
		end
end

function npc:walking()
		if self.t-self.lastshot > self.reloadtime then
			self:shoot()
			self.lastshot = self.t
			 + rnd(self.reloadthresh)
			 --note: with or without random?
		end
end

function npc:shoot()
		direction = (self.player.pos
															-self.pos
															+v(rnd(10), rnd(10)))
															:norm()
															
		
		blt = self:getbullet()
		
		e_add(blt{
    pos=v(self.pos.x, self.pos.y),
    vel=direction,
    origin=self,
    currchange=self:getcurrchange()})
end

function npc:getbullet()
	return bullet
end

function npc:getcurrchange() 
			return min(1, self.t/self.changetime)
end

function npc:take_hit(dmg)
  self.health-=dmg
    shake+=10

    if(self.health <= 0) then
      self.done = true
      explode(
        self.pos.x, self.pos.y,
        self.hitbox.xr-self.hitbox.xl,
        self.hitbox.yb-self.hitbox.yt)
    end
end

-------------------------------
-- entity: bullet
-------------------------------

bullet=changeable:extend({
		speed=2,
    hitbox=box(2,2,6,6),
    sprite=3,
    vel=v(0,0),
    t=0,
    lifetime=30,
    
    sprchange={8,9,10},
    changetime=10,
    currchange=0,
    
    tags={"bullet"},
    collides_with={"player",
    															"npc"},
    dmg=1
})

function bullet:init()
		if self.shtangle ~= nil then
	  self.vel=v(
	    cos(self.shtangle/360),
	    -sin(self.shtangle/360))
		end
		
		self.vel *= self.speed
end

function bullet:update()
  if (self.t > self.lifetime) self.done = true
end

function bullet:collide(e)
		if e~=self.origin then
			self.done=true
			self:explode()

      if (self.hit) self:hit(e)
		end
end

function bullet:getcurrchange()
		return self.currchange
end

function bullet:explode()
	sfx(1)
	for i=1,10 do
		add_new_dust(self.pos.x+4,self.pos.y+4,
			rnd(2)-1,rnd(2)-1,
			10,rnd(2)+1,0.05,0.9,
			{7,7,7,7,7,7,6,6,6,6,6,5,5,9,9,10,10,10,10,10,8,8,8,8})
	end
  shake += 5
end

function bullet:hit(e)
  if e.take_hit then
    e:take_hit(self.dmg)
  end
end

-------------------------------
-- entity: enemy
-------------------------------

enemy=npc:extend({
    sprite=4
})

function enemy:getbullet()
		return enemybullet
end

-------------------------------
-- entity: enemybullet
-------------------------------

enemybullet=bullet:extend({
			sprite=3,
			collides_with={"player"}
})

function enemybullet:hit(e)
  if e:is_a("player") and e.take_hit then
    e:take_hit(self.dmg)
  end
end

-------------------------------
-- entity: friend
-------------------------------

friend=npc:extend({
    sprite=11
})


function friend:getbullet()
		return friendbullet
end

-------------------------------
-- entity: friend bullet
-------------------------------

friendbullet=bullet:extend({
			sprite=12,
   collides_with={"player"}
})

function friendbullet:explode()
	sfx(0)
	for i=1,10 do
		add_new_dust(self.pos.x+4,self.pos.y+4,
			rnd(1)-1,rnd(1)-1,
			10,rnd(3)+1,0.05,0.9,
			{11,11,11,11,11,11,3,3,3,3,3,5,5,3,3,11,11,11,11,11,12,12,12,12})
	end
end

function friendbullet:hit(e)

end

__gfx__
000000000000000000000000000000008888888800000000006666006666666600000000000000000000000000bbbb0000000000000000000000000008550000
000000000066660000000000000aa000888888880000000006666660666666660000000000000000000770000bbbbbb0000cc0000000000008550000085ccc00
00700700060000600085500000a99a0088888888006666006666666666666666000000000006600000766700bb0bb0bb00cbbc000006000000cccc0000c66675
0007700006c00c60000666000a9999a088088088000660006606606666066066000660000066660007666670bb0bb0bb0cbbbbc00006600008566750085bb675
0007700006000060000666000a9999a08a8008a8006006006660066666600666000660000066660007666670bab00bab0cbbbbc00006600008566750085bb675
00700700060cc0600085500000a99a0088888888006666006666666666666666000000000006600000766700bbbbbbbb00cbbc000006000000cccc0000c66675
000000000066660000000000000aa000888888880000000006666660666666660000000000000000000770000bbbbbb0000cc0000000000008550000085ccc00
000000000000000000000000000000008888888800000000006666006666666600000000000000000000000000bbbb0000000000000000000000000008550000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000008855555000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000008855ccccc0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000ccc6c5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000885cc66675000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000088553bbb75990000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000088553bbb75990000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000885cc66675000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000ccc6c5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000008855ccccc0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000008855555000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
00030000000000e0500b0500905007050050500b0501e0502b0501b0001c0001c0001a00019000190001c0001f000230000500001000230001c000120000f0000c0000a0000800007000070000f0001f0002b000
0001000010150101500f1500d1500c1500c1500c1500c1500f0501105012050120500a6500a6500a6500965007650000000000000000000000000000000000000000000000000000000000000000000000000000
